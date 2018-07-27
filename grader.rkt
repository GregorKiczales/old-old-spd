#lang racket

(require "tags.rkt")
(require "dom.rkt") 

(require handin-server/utils)
(require (only-in handin-server/checker add-header-line! submission-eval))

(provide grade-submission
	 grade-problem
	 grade-htdf
	 grade-htdf-coherence
	 grade-signature
	 grade-template
	 grade-submitted-tests
	 grade-additional-tests
	 grade-submitted-tests-adjective

	 weight
	 score-max
	 score-plus

	;grade-htdd
	;grade-htdw
	 )

;; Grader for SPD problems.
;;  grader | problem | htdf | htdw | htdd | fn-defn ... all take list of weight
;;


;; grade-submission expects to be running from within a checker defined by handin/handin-server/checker.
;; it gets the filename that #:create-text? creates, and re-reads the plain text from there.  This means
;; that it can't run tests it pulls out of an htdf element, since some of their images and other values
;; will be replaced.

(define (text->file fn)
  (file (parse-elts (file->top-level-syntax fn))))

(define (file->top-level-syntax fn)
  (parameterize ([read-accept-reader #t]
		 [read-case-sensitive #t]
		 [read-decimal-as-inexact #f])

		(with-input-from-file fn read-top-level-syntax)))

(define (read-top-level-syntax)
  (let ([stx (read-syntax)])
    (if (eof-object? stx)
	'()
	(cons stx (read-top-level-syntax)))))


(define context (make-parameter #f))  ;Element;  the current enclosing element (file, problem, htdf etc.)



;; !!! error handling  (errors -> failed submission)
;; !!! make this provide score, and remind students they can re-submit
;; !!! make order of arguments easier to read (weight first)
(define-syntax (grade-submission stx)
  (syntax-case stx ()
    [(_ fn item ...)
     #'(parameterize ([context (text->file fn)])
	(report-score
	  (combine-scores ""
			  (list item ...))))
     ]))
                   

;; !!! make grade-xxx fail softer
(define-syntax (grade-problem stx)
  (syntax-case stx ()
    [(_ w n item ...)
     #'(parameterize ([context (get-problem* n (context))])
         	     (weight w
		     (combine-scores (format "In (@Problem ~a), " n)
                     		         (list item ...))))]))

(define-syntax (grade-htdf stx)
  (syntax-case stx ()
    [(_ w n item ...)
     #'(parameterize ([context (get-htdf* 'n (context))])
         (weight w
		 (combine-scores (format "in ~a, " (cons '@HtDF (htdf-names (context))))
				 (list item ...))))]))

(define-syntax (grade-htdf-coherence stx)
  (syntax-case stx ()
    [(_ w)
     #'(if (not (htdf? (context)))
	   (error "this would be internal")
	   (weight w
		   (check-htdf-coherence (context))))]))


(define-syntax (grade-signature stx)
  (syntax-case stx ()
    [(_ w n sol)
     #'(parameterize ([context (list-ref (htdf-sigs (context)) n)])
	 (weight w (check-signature (context) 'sol)))]))


(define-syntax (grade-template stx)
  (syntax-case stx ()
    [(_ w n sol)
     #'(parameterize ([context (list-ref (htdf-templates (context)) n)])
         (weight w (check-template (context) 'sol)))]))

;; add additional param to require certain number of tests
(define-syntax (grade-submitted-tests stx)
  (syntax-case stx ()
    [(_ w n min)
     #'(let* ([fn-name (list-ref (htdf-names (context)) n)]
	      [tests    (filter (lambda (t)
				  (eqv? (caadr t) fn-name))
				(htdf-checks (context)))])

	 (weight w (check-tests min "submitted" tests)))]))

(define-syntax (grade-additional-tests stx)
  (syntax-case stx ()
    [(_ w n test ...)
    #'(weight w (check-tests 0 "additional" (list 'test ...)))]))

(define-syntax (grade-submitted-tests-adjective stx)
  (syntax-case stx ()
    [(_ w n define)
     #'(let* ([fn-name (list-ref (htdf-names (context)) n)]
	      [tests    (filter (lambda (t)
				  (eqv? (caadr t) fn-name))
				(htdf-checks (context)))]
	      [new-fn-name (gensym)]
	      [new-tests (subst new-fn-name fn-name tests)])

	 ((submission-eval) (subst new-fn-name fn-name 'define))

	 (weight w (check-tests 0 "submitted against our definition" new-tests)))]))

(define (subst new old in)
  (cond [(eqv? in old) new]
	[(pair? in) (cons (subst new old (car in))
			  (subst new old (cdr in)))]
	[else in]))


(define (get-problem* n c) (scan-elts (lambda (x) (and (problem? x) (=      n (problem-num x)))) (file-elts c)))
(define (get-htdf*    n c) (scan-elts (lambda (x) (and (htdf?    x) (member n (htdf-names x))))  (problem-elts c)))
;!!! htdd...



;; !!! does weight go in here or is it a separate struct
(struct score (v m rpts))

(define (report-score s)
  (add-header-line! (format "Autograding score ~a out of ~a." (score-v s) (score-m s)))
  (for-each add-header-line! (score-rpts s)))


(define (combine-scores prefix scores)
  (score (foldr + 0 (map score-v scores))
         (foldr + 0 (map score-m scores))
	 ;; !!! this could do nicer formatting with indentations or something
         (map (curry string-append prefix)
	      (foldr append '() (map score-rpts scores)))))

(define (weight n . items)
  ;; !!! this needs to do the actual weighting and error checking
  (foldr score-plus (first items) (rest items))) 

(define (score-max n . scores)
  (weight n 
	  (foldr (lambda (s1 s2)
		   (if (not (= (score-m s1) (score-m s2)))
		       (error (format "~a and ~a should be out of the same max." s1 s2))
		       (if (> (score-m s1) (score-m s2))
			   s1
			   s2)))
		 (first scores)
		 (rest scores))))

(define (score-plus s1 s2)
  (score (+ (score-v s1) (score-v s2))
         (+ (score-m s1) (score-m s2))
         (append (score-rpts s1) (score-rpts s2))))


                      
                                            
(define-syntax (rubric stx)
  (syntax-case stx ()
    [(_ [V Q T . FMT-ARGS] ...)
     #'(let* ([max-score (+ V ...)]
              [items     (list (list V (lambda () Q) (format T . FMT-ARGS)) ...)]
              [applied   (filter (lambda (i) (not ((cadr i)))) items)])
         (score (- max-score (foldr + 0 (map car applied))) 
		max-score
                (foldr cons empty (map caddr applied))))]))

(define (check-htdf-coherence htdf)
  (let ()
    (if (not (= (length (htdf-names htdf))
		(length (htdf-sigs htdf))
		(length (htdf-templates  htdf))))
	(score 0 3;!!! fix number
	       "number of fn names, @signatures and @templates is not the same")
	;; check number of args in signature and fn defn and tests
	;; check...
	(score 3 3 (list "???")))))


;; Signature (listof sexp) -> Score
;; Assume solution is well-formed!
;; Signature (listof Symbol) -> Natural
;;  1 correct number of arguments
;;  1 correct arguments types
;;  1 correct result type (including fail?)
(define (check-signature sub sol)
  (let ([sol-args   (reverse (cdr (member '-> (reverse sol))))]
        [sol-result (cadr (member '-> sol))]
        [sol-fail  (equal? (cddr (member '-> sol)) '(or false))])

    (rubric [1 (= (length (signature-args sub)) (length sol-args)) "incorrect number of arguments"]
            [1 (equal? (signature-args sub) sol-args)              "incorrect argument types"]
            [1 (and (eqv? (signature-result sub) sol-result)
                    (eqv? (signature-fail? sub) sol-fail))         "incorrect result type"])))


(define (check-template sub sol)
  (let* ((non-type-origin? (curryr member NON-TYPE-TEMPLATE-ORIGINS))
         (type-origin?     (compose not non-type-origin?))
         [sub-non-types (filter non-type-origin? (template-origins sub))]
         [sol-non-types (filter non-type-origin? sol)]
         [sub-types     (filter     type-origin? (template-origins sub))]
         [sol-types     (filter     type-origin? sol)])
    (rubric [1 (set-subset? sol-types     sub-types)     "template is missing types"]
            [1 (set-subset? sub-types     sol-types)     "template includes incorrect types"]
            [1 (set-subset? sol-non-types sub-non-types) "template is missing non-type origins"]
            [1 (set-subset? sub-non-types sol-non-types) "template includes incorrect type origins"])))

(define (check-tests min source test-exprs)
  (let* ([results (map (submission-eval) test-exprs)]
	 [ntests (length test-exprs)]
	 [npass  (length results)]
	 [enough
	  (cond [(= min 0)       (score 0 0 '())]
		[(>= ntests min) (score 1 1 '())]
		[else
		 (score 0 1 (list (format "insufficient ~a tests, at least ~a required" source min)))])]
	 [corr
	  (score npass
		 ntests
		 (cond [(= ntests npass) '()]
		       [(= ntests 1) (list (format "sole ~a test failed" source))]
		       [else
			(list (format "~a of ~a ~a tests failed" (- ntests npass) ntests source))]))])

    (score-plus enough corr)))


(define (set-equal? s1 s2)
  (and (andmap (curryr member s2) s1)
       (andmap (curryr member s1) s2)))

(define (set-subset? s1 s2)
  (andmap (curryr member s2) s1))

(define (set-superset? s1 s2)
  (ormap (lambda (o) (not (member o s2))) s1))


(define (check-submission-tests x) (score 0 0 ""))
(define (check-submission-function x) (score 0 0 ""))
