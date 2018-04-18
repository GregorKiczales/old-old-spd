#lang racket
(require syntax/readerr)
(require racket/function)
(require rackunit)
 
(provide (all-defined-out)) ;!!! trim this


(define PRIMITIVE-TYPES '(Number Integer Natural String Boolean Image))
(define NUMBER-TYPES    '(Number Integer Natural))

(struct type-comment  (name type)           #:transparent)
(struct type ()                             #:transparent)
(struct atomic-type   type (name)           #:transparent)
(struct interval-type type (nt lc? hc? l h) #:transparent)
(struct one-of-type   type (cases)          #:transparent)
(struct distinct-type type (val)            #:transparent)
(struct ref-type      type (name)           #:transparent)
(struct compound-type type (cns fields)     #:transparent)
(struct listof-type   type (elt-type)       #:transparent)
(struct function-type type (args result)    #:transparent) ;!!! add fail?





(define (read-type-comment-string str) (read-type-comment (open-input-string str)))
(define (read-type-string         str) (read-type         (open-input-string str)))

(define (read-type-comment in)
  (let ([tn (read in)])
    (when (eof-object? tn)
      (raise-read-error "Expected typename followed by ' is'."))
    (let ([is (read in)])
      (unless (eqv? is 'is)
        (raise-read-error (printf "Expected 'is' after ~a." tn)))
      (type-comment tn (read-type in)))))
        

(define (read-type in)
  (let ([read-as-char (case-lambda
                        [(ch in) ch]
                        [(ch in src line col pos) (datum->syntax ch src line col pos)])])
    (parameterize ([current-readtable 
                    (make-readtable (current-readtable)
                                    #\[ 'terminating-macro read-as-char 
                                    #\( 'terminating-macro read-as-char                 
                                    #\, 'terminating-macro read-as-char                 
                                    #\] 'terminating-macro read-as-char
                                    #\) 'terminating-macro read-as-char)])
      (read-type-internal in false))))

(define (read-type-internal in recursive?)
  (define-values (line col pos) (port-next-location in))
  (let ([raw (read in)])
    (print raw) (print (equal? raw '#\())
    (match raw
      [(? eof-object?)              raw]      
      
      [(? number-type-name?    tn) (read-interval tn in)]
      [(? primitive-type-name? tn) (atomic-type tn)]
      [(? distinct-value?      v)  (build-distinct-type v)]
      [(? one-of?              kw) (read-one-of kw in recursive?)]
      [`->                         '->]
      [(? symbol?              tn) (ref-type tn)]

      [#\(                         (read-list-syntax in recursive?)]

      [_ (raise-read-error "Bad type synax." in line col pos (and pos (- (file-position in) pos)))])))

      

(define number-type-name?    (curryr member NUMBER-TYPES))
(define primitive-type-name? (curryr member PRIMITIVE-TYPES))
(define one-of?              (curryr member '(one oneof oneof: one-of one-of:)))

(define (distinct-value? x)
  (or (string? x)
      (member x '(true false empty '()))))

(define (build-distinct-type x)
  (distinct-type
   (if (string? x)
       x
       (second (assoc x `((true  ,true)
                          (false ,false)
                          (empty ,empty)
                          (()    ,empty)))))))
      

;; -> Type
;; !!! needs better handling of cases where something is bad after lb
(define (read-interval nt in)
  (if (not (regexp-match-peek #px"^(\\[|\\()" in))
      (atomic-type nt)
      (let ([lb (read in)]
            [lo (read in)]
            [x1 (regexp-match #px"\\," in)]   ; skip comma
            [hi (read in)]
            ;[x2 (regexp-match #px"^\\s*" in)] ; skip whitespace
            [hb (read in)])
        ;(unless (member hb '(#\] #\)))
        ;  (bad-ending lb hb src in))
        (interval-type nt (eqv? lb #\[) (eqv? hb #\]) lo hi))))

(define (read-one-of kw in recursive?)
  (define-values (l c p) (port-next-location in))
  (when recursive?
    (raise-read-error "Can't nest a one of in another type in a type comment." in l c p p))
  ;; Provide some correction on spelling of "one of:"  
  (cond [(eqv? kw 'one)    (unless (regexp-match #px"^\\s*of:" in) (raise-read-error "Expected ' of:\n' after 'one'." in l c p p))]
        [(eqv? kw 'oneof)                                          (raise-read-error "Expected 'one of:\n'." in l c p p)]
        [(eqv? kw 'one-of)                                         (raise-read-error "Expected 'one of:\n'." in l c p p)]
        [(eqv? kw 'oneof:)                                         (raise-read-error "Expected 'one of:\n'." in l c p p)]
        [(eqv? kw 'one-of:)                                        (raise-read-error "Expected 'one of:\n'." in l c p p)])
        
  (let loop ([result '()])
    (read-line in) ;(regexp-match #rx"\\n" in)             ;eat the rest of the line
    (if (not (regexp-match-peek #px"\\s*\\;*\\s*\\-" in))  ;not something like ;; -
        (if (empty? result)
            (let-values ([(l c p) (port-next-location in)])
              (raise-read-error "After one of: expected at least one new line and ;; - and a type." in l c p p))
            (one-of-type (reverse result)))
        (begin
          (regexp-match #px"\\s*\\;*\\s*\\-" in)
          (let ([t (read-type-internal in #t)])
            (if (eof-object? t)
                (let-values ([(l c p) (port-next-location in)])
                  (raise-read-error "Expected to find a type after -." in l c p p))
                (loop (cons t result))))))))

;; <type> ...)
;; <type> ... -> <type>)
(define (read-list-syntax in recursive?)
  (define-values (line col pos) (port-next-location in))
  (let loop ([result '()])
    (let ([close? (regexp-match-peek #px"^\\s*\\)" in)])
      (cond [(and close? (empty? result))     (raise-read-error "Expected a type name after '('." in line pos 1)]
            [(and close? (member '-> result))
             (regexp-match #px"^\\s*\\)" in)
             (function-type (reverse (rest (rest result))) (first result))]
            [close?
             (regexp-match #px"^\\s*\\)" in)
             (compound-type (ref-type-name (last result)) (rest (reverse result)))]
            [else
             (let ([t (read-type-internal in #t)])
               (if (eof-object? t)
                   (raise-read-error "Expected to find a type or )")
                   (loop (cons t result))))]))))


(check-equal? (read-type-string "Number")            (atomic-type 'Number))
(check-equal? (read-type-string "Integer")           (atomic-type 'Integer))
(check-equal? (read-type-string "Natural")           (atomic-type 'Natural))

(check-equal? (read-type-string "Number[1 , 2)")     (interval-type 'Number #t #f 1 2))
(check-equal? (read-type-string "Natural(1 , 2]")    (interval-type 'Natural #f #t 1 2))
(check-equal? (read-type-string "Integer(1 , 2)")    (interval-type 'Integer #f #f 1 2))
(check-equal? (read-type-string "Number[1,2]")       (interval-type 'Number #t #t 1 2))
(check-equal? (read-type-string "Number[1.0 , 2.0]") (interval-type 'Number #t #t 1.0 2.0))

(check-equal? (read-type-string "\nNumber[1.0\n,\n2.0 \n]") (interval-type 'Number #t #t 1.0 2.0))

(check-equal? (read-type-string "Boolean")           (atomic-type 'Boolean))

(check-equal? (read-type-string "Foo")               (ref-type 'Foo))


(check-equal? (read-type-string "\"\"")             (distinct-type ""))
(check-equal? (read-type-string "\"red\"")          (distinct-type "red"))
(check-equal? (read-type-string "true")             (distinct-type true))
(check-equal? (read-type-string "false")            (distinct-type false))
(check-equal? (read-type-string "empty")            (distinct-type empty))
;(check-equal? (read-type-string "'()")              (distinct-type empty)) !!!

(check-equal? (read-type-string "one of: \n ;; - Foo\n ;; - Number")
              (one-of-type (list (ref-type 'Foo) (atomic-type 'Number))))

(check-equal? (read-type-string "one of: \n ;; - Foo noise\n ;; - Number  noise")
              (one-of-type (list (ref-type 'Foo) (atomic-type 'Number))))

(check-equal? (read-type-string "( X Y -> Z )")
              (function-type (list (ref-type 'X) (ref-type 'Y)) (ref-type 'Z)))


(check-equal? (read-type-string "( X (x -> y) -> y )")
              (function-type (list (ref-type 'X)
                                   (function-type (list (ref-type 'x))
                                                  (ref-type 'y)))
                             (ref-type 'y)))


(check-equal? (read-type-string "(make-foo Number Number)")
              (compound-type 'make-foo (list (atomic-type 'Number) (atomic-type 'Number))))

(check-equal? (read-type-string "(make-foo Number Number[1, 2] Number)")
              (compound-type 'make-foo (list (atomic-type 'Number)
                                             (interval-type 'Number #t #t 1 2)
                                             (atomic-type 'Number))))



(check-equal? (read-type-string "(make-foo (make-bar Number))")
              (compound-type 'make-foo (list (compound-type 'make-bar (list (atomic-type 'Number))))))

(check-equal? (read-type-string "(make-foo Number (make-bar Number) Foo)")
              (compound-type 'make-foo (list (atomic-type 'Number)
                                             (compound-type 'make-bar (list (atomic-type 'Number)))
                                             (ref-type 'Foo))))

(check-equal? (read-type-string "(make-foo (X -> Y))")
              (compound-type 'make-foo (list (function-type (list (ref-type 'X)) (ref-type 'Y)))))

(check-equal? (read-type-string "(make-foo (X -> Y) Number)")
              (compound-type 'make-foo (list (function-type (list (ref-type 'X)) (ref-type 'Y))
                                             (atomic-type 'Number))))

(check-equal? (read-type-string "(make-foo Number (X -> Y))")
              (compound-type 'make-foo (list (atomic-type 'Number)
                                             (function-type (list (ref-type 'X)) (ref-type 'Y)))))

 
