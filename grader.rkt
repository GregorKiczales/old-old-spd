#lang racket

(require "tags.rkt")
(require "dom.rkt")

(require handin-server/utils)
(require (only-in handin-server/checker add-header-line!))

(provide (all-defined-out)) ;!!!

;; Grader for SPD problems.
;;  grader | problem | htdf | htdw | htdd | fn-defn ... all take list of weight
;;




(struct score (v m rpts) #:transparent) 

(define (score-max s1 s2)
  (if (not (= (score-m s1) (score-m s2)))
      (error (format "~a and ~a should be out of the same max." s1 s2))
      (if (> (score-m s1) (score-m s2))
          s1
          s2)))

(define (score-plus s1 s2)
  (score (+ (score-v s1) (score-v s2))
         (+ (score-m s1) (score-m s2))
         (append (score-rpts s1) (score-rpts s2))))


;; grade-submission is handed the filename to grade, it expects to be running from within
;; a checker defined by handin/handin-server/checker.

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


(define context       (make-parameter #f))  ;Element;  the current enclosing element (file, problem, htdf etc.)


;; !!! error handling

(define-syntax (grade-submission stx)
  (syntax-case stx ()
    [(_ fn item ...)
     #'(parameterize ([context (text->file fn)])
	(report-score
	  (combine-scores ""
			  (list item ...))))
     ]))
                   

(define-syntax (grade-problem stx)
  (syntax-case stx ()
    [(_ n weight item ...)
     #'(parameterize ([context (get-problem* n (context))])
         (combine-scores (format "In (@problem ~a), " n)
                         (list item ...)))]))

(define-syntax (grade-htdf stx)
  (syntax-case stx ()
    [(_ n weight item ...)
     #'(parameterize ([context (get-htdf* n (context))])
         (combine-scores (format "in (@HtDF ~a), " n)
                         (list item ...)))]))


(define-syntax (grade-signature stx)
  (syntax-case stx ()
    [(_ n weight sol)
     #'(parameterize ([context (list-ref (htdf-sigs (context)) n)]) ;make this fail softer
         (check-signature (context) sol))]))


(define-syntax (grade-template stx)
  (syntax-case stx ()
    [(_ n weight sol)
     #'(parameterize ([context (list-ref (htdf-templates (context)) n)]) ;make this fail softer
         (check-template (context) sol))]))


;; !!!
(define (get-problem* n c) (scan-elts (lambda (x) (and (problem? x) (=      n (problem-num x)))) (file-elts c)))
(define (get-htdf*    n c) (scan-elts (lambda (x) (and (htdf?    x) (member n (htdf-names x))))  (problem-elts c)))


;htdd...



(define (report-score s)
  (add-header-line! (format "Autograding score ~a out of ~a." (score-v s) (score-m s)))
  (for-each  add-header-line! (score-rpts s)))

(define (combine-scores prefix scores)
  (score (foldr + 0 (map score-v scores))
         (foldr + 0 (map score-m scores))
         (map (curry string-append prefix) (foldr append '() (map score-rpts scores)))))

(define (weight n . items)
  (foldr score-plus (first items) (rest items))) ;!!!


                      
                                            
(define-syntax (rubric stx)
  (syntax-case stx ()
    [(_ [V Q T . FMT-ARGS] ...)
     #'(let* ([max-score (+ V ...)]
              [items     (list (list V (lambda () Q) (format T . FMT-ARGS)) ...)]
              [applied   (filter (lambda (i) (not ((cadr i)))) items)])
         (score (- max-score (foldr + 0 (map car applied))) 
		max-score
                (foldr cons empty (map caddr applied))))]))


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

(define (set-equal? s1 s2)
  (and (andmap (curryr member s2) s1)
       (andmap (curryr member s1) s2)))

(define (set-subset? s1 s2)
  (andmap (curryr member s2) s1))

(define (set-superset? s1 s2)
  (ormap (lambda (o) (not (member o s2))) s1))


(define (check-submission-tests x) (score 0 0 ""))
(define (check-submission-function x) (score 0 0 ""))
