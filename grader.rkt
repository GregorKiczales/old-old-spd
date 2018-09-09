#lang racket

(require "tags.rkt")
(require "dom.rkt")
(require (only-in racket/gui open-input-graphical-file))

(provide grade-submission
         grade-problem
         
         grade-htdf
         grade-htdf-coherence
         grade-signature
         grade-template
         grade-submitted-tests
         grade-additional-tests
         grade-tests-thoroughness

         grade-htdd
         grade-htdd-coherence
         grade-dd-template-rules

         check-htdf-coherence
         check-signature
         check-template
         check-tests

         check-htdd-coherence
         check-dd-template-rules
         
         weight
         score-max
         score

         context

         text->file
         subst
)

;; Grader for SPD problems.
;;  grader | problem | htdf | htdw | htdd | fn-defn ... all take list of weight


;; grade-submission expects to be running from within gradefn. It expectes a filename from :create-text?,
;; a username (as a string), an evaluator (wrapped, if needed), a reporter function (takes a string), and
;; a place to log internal errors

(struct exn:fail:internal exn:fail () #:transparent)
(struct exn:fail:student exn:fail () #:transparent)

(define (raise-internal-error msg . v)
  (raise (exn:fail:internal (apply format msg v) (current-continuation-marks))))

(define (raise-student-error msg . v)
  (raise (exn:fail:student     (apply format msg v) (current-continuation-marks))))

(define-syntax (recovery-point stx)
  (syntax-case stx ()
    [(_ w exp ...)
     #'(with-handlers ([void (lambda (e)
                               (unless (exn:fail:student? e)
				       ((logger) (string-append (if (exn:fail:internal? e) "Internal Error:" "Unclassified Error:")
								(exn-message e))))
                               (score w 0 (list (exn-message e))))])
          exp ...)]))

(define (check-context p who str)
  (unless (p (context))
    (raise-internal-error "~a is used in a ~a context." who str)))

(define (check-bounds n max kind)
  (unless (valid-index (sub1 n) max) (raise-student-error "could not find the ~a ~a" (number->ordinal n) kind)))

;; reading utility functions

(define (text->file fn)
  (file (parse-elts (file->top-level-syntax fn))))

(define (file->top-level-syntax fn)
  (parameterize ([read-accept-reader #t]
                 [read-case-sensitive #t]
                 [read-decimal-as-inexact #f]
                 [current-input-port (open-input-graphical-file fn)])
    (read-top-level-syntax)))

(define (read-top-level-syntax)
  (let ([stx (read-syntax)])
    (if (eof-object? stx)
        empty
        (cons stx (read-top-level-syntax)))))

(define context   (make-parameter #f)) ;Element;  the current enclosing element (file, problem, htdf etc.)
(define evaluator (make-parameter #f)) ;the submission evaluator
(define reporter  (make-parameter #f)) ;fn to write grading messages to (goes to student)
(define logger    (make-parameter #f)) ;logger for internal errors      (goes to staff)


;; grade-* syntax

(define-syntax (grade-submission stx)
  (syntax-case stx ()
    [(_ fn user evl rpt log item ...)
     #'(parameterize ([logger log])
					;(check-context false? "non-global")  !!! fix this later
	 (parameterize ([context (text->file fn)]
			[evaluator evl]
			[reporter  rpt])
	   (let ([s (recovery-point 1 (weight 1 (combine-scores "" (list item ...))))])
	     (report-score s)
	     (format "Autograding grade is ~a." (pretty-print-percent (score-v s))))))]))


(define-syntax (grade-problem stx)
  (syntax-case stx ()
    [(_ w n item ...)
     #'(recovery-point w
         (check-context file? 'grade-problem "non-file")
         (parameterize ([context (get-problem* n (context))])
           (weight w (combine-scores (format "In (@Problem ~a), " n) (list item ...)))))]))

(define-syntax (grade-htdf stx)
  (syntax-case stx ()
    [(_ w n item ...)
     #'(recovery-point w
         (check-context problem? 'grade-htdf "non-problem")
         (parameterize ([context (if (number? 'n)
                                     (let ([htdf-list (filter htdf? (problem-elts (context)))])
                                       (check-bounds 'n (length htdf-list) "HtDF")
                                       (list-ref htdf-list (sub1 'n)))
                                     (get-htdf* 'n (context)))])
           (weight w (combine-scores (format "in ~a, " (cons '@HtDF (htdf-names (context)))) (list item ...)))))]))

(define-syntax (grade-htdf-coherence stx)
  (syntax-case stx ()
    [(_ w)
     #'(recovery-point w
         (check-context htdf? 'grade-htdf-coherence "non-HtDF")
         (weight w
                 (check-htdf-coherence (context))))]))

(define-syntax (grade-signature stx)
  (syntax-case stx ()
    [(_ w n sol)
     #'(recovery-point w
         (check-context htdf? 'grade-signature "non-HtDF")
         (check-bounds n (length (htdf-sigs (context))) "signature")
         (parameterize ([context (list-ref (htdf-sigs (context)) (sub1 n))])
           (weight w (check-signature (context) 'sol))))]))

(define-syntax (grade-template stx)
  (syntax-case stx ()
    [(_ w n sol)
     #'(recovery-point w
         (check-context htdf? 'grade-template "non-HtDF")
         (check-bounds n (length (htdf-templates (context))) "template")
         (parameterize ([context (list-ref (htdf-templates (context)) (sub1 n))])
           (weight w (check-template (context) 'sol))))]))

(define-syntax (grade-submitted-tests stx)
  (syntax-case stx ()
    [(_ w n min)
     #'(recovery-point w
         (check-context htdf? 'grade-submitted-tests "non-HtDF")
         (check-bounds n (length (htdf-names (context))) "function")
         (let* ([fn-name (list-ref (htdf-names (context)) (sub1 n))]
                [tests   (filter (lambda (t)
                                   (let ([test-actual (cadr t)])
                                     (or (eqv? (car test-actual) fn-name)
                                         (and (eqv? (car test-actual) 'local)
                                              (eqv? (caaddr test-actual) fn-name)))))
                                 (htdf-checks (context)))])
           (weight w (check-tests min "submitted" tests))))]))

(define-syntax (grade-additional-tests stx)
  (syntax-case stx ()
    [(_ w n test ...)
     #'(recovery-point w
         (check-context htdf? 'grade-additional-tests "non-HtDF")
         (weight w (check-tests 0 "additional"
				 (list 'test ...))))]))

;; defined function(s) should fail at least one test.
(define-syntax (grade-tests-thoroughness stx)
  (syntax-case stx ()
    [(_ w n defines ...)
     #'(recovery-point w
         (check-context htdf? 'grade-tests-thoroughness "non-HtDF")
         (check-bounds n (length (htdf-names (context))) "function")
	 (check-thoroughness w n (list 'defines ...)))]))

(define (check-thoroughness w n all-defs)
  (let* ([fn-name (list-ref (htdf-names (context)) (sub1 n))]
	 [tests (filter (lambda (t) (eqv? (caadr t) fn-name))
			(htdf-checks (context)))]
	 [nfuns (length all-defs)])
    (let loop ([defs all-defs]
	       [ndetected 0])
      
      (if (empty? defs)
	  (score w
		 (/ ndetected nfuns)
		 (if (= ndetected nfuns)
		     '()
		     (list (format "submitted tests failed to detect ~a out of our ~a buggy functions"
				   (- nfuns ndetected)
				   nfuns))))
	
	  (let* ([new-fn-name (gensym)]
		 [new-tests (subst new-fn-name fn-name tests)])	    ;!!! hygiene goes right out the window
	    ((evaluator) (subst new-fn-name fn-name (first defs)))  ;!!! hygiene goes right out the window
	    
	    (let ([result (check-tests 0 "submitted against our definition" new-tests)])
	      (loop (rest defs)
		    (if (ormap (compose not eval-check) new-tests)
			(add1 ndetected)
			ndetected))))))))


(define-syntax (grade-htdd stx)
  (syntax-case stx ()
    [(_ w n item ...)
     #'(recovery-point w
         (check-context problem? 'grade-htdd "non-problem")
         (parameterize ([context (if (number? 'n)
                                     (let ([htdd-list (filter htdd? (problem-elts (context)))])
                                        (if (not (valid-index (sub1 'n) (length htdd-list)))
                                            (raise-student-error (format "could not find the find the ~a HtDD." (number->ordinal 'n)))
                                            (list-ref htdd-list (sub1 'n))))
                                     (get-htdd* 'n (context)))])
           (weight w (combine-scores (format "in ~a, " (cons '@HtDD (htdd-names (context)))) (list item ...)))))]))

(define-syntax (grade-dd-template-rules stx)
  (syntax-case stx ()
    [(_ w n sol)
     #'(recovery-point w
         (check-context htdd? 'grade-dd-template-rules "non-HtDD")
         (check-bounds n (length (htdd-rules (context))) "template rules")
         (parameterize ([context (list-ref (htdd-rules (context)) (sub1 n))])
           (weight w (check-dd-template-rules (context) 'sol))))]))

(define-syntax (grade-htdd-coherence stx)
  (syntax-case stx ()
    [(_ w)
     #'(recovery-point w
         (check-context htdd? 'grade-htdd-coherence "non-HtDD")
         (weight w (check-htdd-coherence (context))))]))

;; get- utility functions

(define (get-problem* n c) (or (scan-elts (lambda (x) (and (problem? x) (= n (problem-num x)))) (file-elts c))
                               (raise-student-error (format "Could not find Problem ~a." n))))
(define (get-htdf* n c) (or (scan-elts (lambda (x) (and (htdf? x) (member n (htdf-names x)))) (problem-elts c))
                            (raise-student-error (format "could not find the HtDF for ~a." n))))
(define (get-htdd* n c) (or (scan-elts (lambda (x) (and (htdd? x) (member n (htdd-names x)))) (problem-elts c))
                            (raise-student-error (format "could not find the HtDD for ~a." n))))
(define (get-htdw* n c) (or (scan-elts (lambda (x) (and (htdw? x) (eqv? n (htdw-ws x)))) (problem-elts c))
                            (raise-student-error (format "could not find the HtDW for ~a." n))))

;; score utility functions

(struct score (w v rpts))

(define (report-score s)
  ((reporter) (format "Autograding score ~a." (pretty-print-percent (score-v s))))
  (for-each (reporter) (score-rpts s)))

(define (combine-scores prefix scores)
  (let ([total (foldl + 0 (map score-w scores))])
    (unless (= 1 (round* total 2)) (raise-internal-error (format "Scores ~a do not total to a weight of one. (Totals to ~a.)" prefix total)))
    (score 1
           (round* (foldl + 0 (map * (map score-w scores)
                                     (map score-v scores)))
                   2)
           (map (curry string-append prefix) ;; !!! this could do nicer formatting with indentations or something
                (reverse (foldl append empty (map score-rpts scores)))))))

(define (weight n item)
  (struct-copy score item [w n]))

(define (score-max n . scores)
  (weight n 
          (foldl (lambda (s1 s2)
                   (cond [(not (= (score-w s1) (score-w s2))) (raise-internal-error (format "~a (weighted ~a) and ~a (weighted ~a) should be weighted the same." (score-v s1) (score-w s1) (score-v s2) (score-w s2)))]
                         [(> (score-v s1) (score-v s2)) s1]
                         [else s2]))
                 (car scores)
                 (cdr scores))))


;; check-* stuff

;; Helper for check-*
(define-syntax (rubric stx)
  (syntax-case stx ()
    [(_ [V Q T . FMT-ARGS] ...)
     #'(let* ([max-score (+ V ...)]
              [items (list (list V (lambda () Q) (format T . FMT-ARGS)) ...)]
              [applied (filter (lambda (i) (not ((cadr i)))) items)])
         (score 1
                (/ (- max-score (foldl + 0 (map car applied))) max-score)
                (foldl cons empty (map caddr applied))))]))

(define (check-htdf-coherence htdf)
  (let ()
    (rubric [1 (= (length (htdf-names htdf)) (length (htdf-sigs htdf)) (length (htdf-templates htdf)))
             "number of fn names, @signatures and @templates is not the same"])))
     
      
      ;[1 (and (= (length (htdf-sigs htdf)) (length (htdf-defns htdf)))
    ;  (andmap = (map (compose length signature-args) (htdf-sigs htdf))
     ;           (map (compose length cdadr) (htdf-defns htdf))))
      ;"a function signature does not match the function definition"]))
       
     ;[1 () "a template contains a type origin not in your signature."]))) ;; can't handle encapsulated mutual ref.

            ;[1 (template-origins (htdf-template htdf)) "types are mentioned in the template that are not mentioned in the signature"])))

#;
(define (check-htdf-coherence htdf)
  (if (not (= (length (htdf-names htdf))
              (length (htdf-sigs htdf))
              (length (htdf-templates  htdf))))
      (score 1 0 (list "number of fn names, @signatures and @templates is not the same"))
      ;; check number of args in signature and fn defn and tests
      (if ()
          (score 1 0.5 (list ))
          (score 1 1 empty))))

;; Signature (listof sexp) -> Score
;; Assume solution is well-formed!
;; Signature (listof Symbol) -> Natural
;;  1 correct number of arguments
;;  1 correct arguments types
;;  1 correct result type (including fail?)
(define (check-signature sub sol)
  (let ([sol-args (reverse (cdr (member '-> (reverse sol))))]
        [sol-result (cadr (member '-> sol))]
        [sol-fail (equal? (cddr (member '-> sol)) '(or false))])
    (rubric [1 (= (length (signature-args sub)) (length sol-args)) "incorrect number of arguments"]
            [1 (equal? (signature-args sub) sol-args)              "incorrect argument types"]
            [1 (and (equal? (signature-result sub) sol-result)
                    (equal? (signature-fail? sub) sol-fail))         "incorrect result type"])))

(define (check-template sub sol)
  (let* ([non-type-origin? (curryr member NON-TYPE-TEMPLATE-ORIGINS)]
         [type-origin? (compose not non-type-origin?)]
         [sub-non-types (filter non-type-origin? (template-origins sub))]
         [sol-non-types (filter non-type-origin? sol)]
         [sub-types (filter type-origin? (template-origins sub))]
         [sol-types (filter type-origin? sol)])
    (rubric [1 (set-subset? sol-types sub-types)     "template is missing types"]
            [1 (set-subset? sub-types sol-types)     "template includes incorrect types"]
            [1 (set-subset? sol-non-types sub-non-types) "template is missing non-type origins"]
            [1 (set-subset? sub-non-types sol-non-types) "template includes incorrect type origins"])))

(define (check-tests min source test-exprs)
  (let* ([results (map eval-check test-exprs)]
         [ntests (length test-exprs)]
         [npass (length (filter identity results))]
         [enough
          (cond [(= min 0)       (score 0  0 '())]
                [(>= ntests min) (score .5 1 empty)]
                [else
                 (score .5 0 (list (format "insufficient ~a tests, at least ~a required" source min)))])]
         [corr
          (score (if (= min 0) 1 .5)
                 (if (= ntests 0) 0 (/ npass ntests))
                 (cond [(= ntests npass) empty]
                       [(= ntests 1) (list (format "sole ~a test failed" source))]
                       [else
                        (list (format "~a of ~a ~a tests failed" (- ntests npass) ntests source))]))])
    (combine-scores "" (list enough corr))
    #;
    (if (not (false? enough)) ;; if we are checking for number of tests, combine them. Otherwise, just take correctness as the score
        (score 1
               (+ (* 0.5 (score-v corr))
                  (* 0.5 (score-v enough)))
               (append (score-rpts enough)
                       (score-rpts corr)))
        corr)))

(define (eval-check test-expr)
  (with-handlers ([void (lambda (e)
			  ((logger) (format "Error running test ~s: ~a" test-expr (exn-message e)))
			  false)])
		 (cond [(and (pair? test-expr) (eq? (car test-expr) 'check-expect))
			(equal? ((evaluator) (cadr test-expr))
				((evaluator) (caddr test-expr)))]
		       [(and (pair? test-expr) (eq? (car test-expr) 'check-within))
			(<= (magnitude ((evaluator) (cadr test-expr))
				       ((evaluator) (caddr test-expr)))
			    ((evaluator) (cadddr test-expr)))]
		       [else
			(let ([result ((evaluator) test-expr)])
			  (if (boolean? result)
			      result
			      true))])))  ;this really is a grader file error

(define (check-htdd-coherence htdd)
  (score 1 1 '())
;  (let ()
;    (rubric [1 (= (length (htdd-names htdd)) (length (htdd-templates htdd))) "number of data types and templates is not the same"]))
  )

(define (check-dd-template-rules sub sol)
  (let* ([sub-rules (dd-template-rules-rules sub)]
         [sol-rules sol])
    (rubric [1 (set-subset? sol-rules sub-rules)     "template is missing rules"]
            [1 (set-subset? sub-rules sol-rules)     "template includes incorrect rules"])))


;; Utility functions

(define (subst new old in)
  (cond [(eqv? in old) new]
        [(pair? in) (cons (subst new old (car in))
                          (subst new old (cdr in)))]
        [else in]))

(define (within? a b delta)
  (< (abs (- a b)) (abs delta)))

(define (round* value decimals)
  (/ (round (* (expt 10 decimals) value)) (expt 10 decimals)))

(define (pretty-print-percent p)
  (let ([x (number->string (round (* 100 p)))])
    (if (string-contains? x ".")
        (string-append (substring x 0 (caar (regexp-match-positions #rx"\\." x))) "%")
        (string-append x "%"))))

(define (number->ordinal n)
  (unless (and (not (negative? n)) (integer? n)) (raise-internal-error "Can't convert the number ~a to an ordinal." n))
  (let ([nstr (number->string n)])
    (cond [(and (= 1 (modulo n 10)) (not (= n 11))) (string-append nstr "st")]
          [(and (= 2 (modulo n 10)) (not (= n 12))) (string-append nstr "nd")]
          [(and (= 3 (modulo n 10)) (not (= n 13))) (string-append nstr "rd")]
          [else (string-append nstr "th")])))

(define (valid-index index list-length)
  (and (<= 0 index) (< index list-length)))

;; Set utility functions

(define (set-equal? s1 s2)
  (and (andmap (curryr member s2) s1)
       (andmap (curryr member s1) s2)))

(define (set-subset? s1 s2)
  (andmap (curryr member s2) s1))

(define (set-superset? s1 s2)
  (ormap (lambda (o) (not (member o s2))) s1))
