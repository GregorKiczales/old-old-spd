#lang racket

(require "tags.rkt")
(require "grader-utils.rkt")

(provide (all-defined-out)) ;!!!

;; Grader for SPD problems.
;;  grader | problem | htdf | htdw | htdd | fn-defn ... all take list of weight
;;
#;
(grader "Problem Set 3"
        (weight .60
                (problem 1
                         (htdf name|num|pred
                               (choose-max 
                                (grade-signature htdf '(X Y Z))
                                (grade-signature htdf '(A B C))))))
  
        (weight .40
                (problem 2
                         (grade-signature htdf `(,(get-elt 2 htdd) -> String or false))
                         (grade-template  template '(,(get-elt 2 htdd) backtracking))
                                             
                         (grade-fn definition
                                   (define (pyramid n i)
                                     (cond [(zero? n) empty-image]
                                           [else
                                            (above (pyramid (sub1 n) i)
                                                   (set-row n i))]))
                                   (list '(0 COOKIES)
                                         '(1 COOKIES)
                                         '(3 COOKIES)))
   
                         (grade-checks checks false
                                       (define (pyramid n i)
                                         (cond [(zero? n) empty]
                                               [else
                                                (cons (pyramid (sub1 n) i)
                                                      (set-row n i))]))))))



(struct score (v m rpts) #:transparent)  ;!!! THIS HAS TO GET REPORTS TOO, SO THEY GET SELECTED BASED ON WHICH SCORE IS SELECTED

(define (score-max s1 s2)
  (if (not (= (score-m s1) (score-m s2)))
      (error (format "~a and ~a should have the same max." s1 s2))
      (if (> (score-m s1) (score-m s2))
          s1
          s2)))

(define (score-plus s1 s2)
  (score (+ (score-v s1) (score-v s2))
         (+ (score-m s1) (score-m s2))
         (append (score-rpts s1) (score-rpts s2))))


;(define file-elements (make-parameter '()))
(define context       (make-parameter #f))  ;(listof Element)
(define report-prefix (make-parameter ""))  ;(listof String)   context scoping description in reverse order

(define (report s) (print s))

;; !!! error handling

(define-syntax (grader stx)
  (syntax-case stx ()
    [(_ desc items ...)
     #'(define (checker submission users)
         (parameterize (;[file-elements xx]
                        [context (parse-submission submission)]
                        [report-prefix ""]
                        ;[reports '()]
                        )
           (combine-scores items...)))]))
                   

(define-syntax (problem stx)
  (syntax-case stx ()
    [(_ n desc items ...)
     #'(parameterize ([context (get-problem* n (context))]
                      [report-prefix (cons (format "Problem ~a: " n) report-prefix)]
                      ;[reports '()]
                      )
         (combine-scores items...))]))

(define-syntax (htdf stx)
  (syntax-case stx ()
    [(_ n desc items ...)
     #'(parameterize ([context (get-htdf* n (context))]
                      [report-prefix (string-append report-prefix (format "HtDF ~a: " (htdf-names (context))))]
                      ;[reports '()]
                      )
         (combine-scores items...))]))

;htdd...

(define (combine-scores . items)
  '!!!)

(define (weight n . items)
  '!!!)


                      
                                            
(define-syntax (rubric stx)
  (syntax-case stx ()
    [(_ [V Q T . FMT-ARGS] ...)
     #'(let* ([max-score (+ V ...)]
              [items     (list (list V (lambda () Q) (format T . FMT-ARGS)) ...)]
              [applied   (filter (lambda (i) (not ((cadr i)))) items)])
         (score max-score
                (- max-score (foldr + 0 (map car applied)))
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
;(define (check-submission-tests x) (score 0 0 ""))
  
                          

'signatures
(check-signature (signature '(X) #f #f)    '(X Y -> Z)) 
(check-signature (signature '(Y X) #f #f)  '(X Y -> Z))
(check-signature (signature '(X Y) #f #f)  '(X Y -> Z))
(check-signature (signature '(X Y) 'Z #f)  '(X Y -> Z))
(check-signature (signature '(X Y) 'Z #t)  '(X Y -> Z or false))

'templates
(check-template (template '(Foo)) '(Foo))
(check-template (template '()) '(Foo))
(check-template (template '(Foo Bar)) '(Foo))
(check-template (template '(Bar)) '(Foo))

(check-template (template '(genrec)) '(genrec))
(check-template (template '()) '(genrec))
(check-template (template '(encapsulated genrec)) '(genrec))
(check-template (template '(encapsulated)) '(genrec))
                  
