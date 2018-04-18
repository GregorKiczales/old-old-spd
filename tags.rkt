#lang racket
(require (for-syntax racket/base syntax/parse racket/list))
(require syntax/keyword)

;; Syntax for @tags including basic syntax checking.
;; The tags are further checked through the grader framework.

(provide @Problem          @HtDW
         @HtDD
         @HtDF
          
         @dd-template-rules
         @template)

;          Number Integer Natural String Boolean Image
;
;          ;<TypeName>     decomposition and possible structural recursion
;          add-param       ;used once for 1 or more
;          fn-composition
;          backtracking
;          2-one-of
;          encapsulated
;          use-abstract-fn
;          genrec
;          accumulator
;          for-each))


;; @tags are checked for basic validity during parsing, they expand to
;; just (values) so that they don't affect what values are printed when
;; student code is run.
;; The file structure parser pulls apart the syntax later, knowing they
;; have met minimal syntactic checking.

(define PRIMITIVE-TYPES '(Number Integer Natural String Boolean Image))

(define-for-syntax TEMPLATE-ORIGINS
  '(;<TypeName>      decomposition and possible structural recursion
    add-param        ;used once for 1 or more atomic params
    fn-composition   ;used once for composition of 2 or more fns
    backtracking     
    2-one-of
    encapsulated
    use-abstract-fn
    genrec
    accumulator
    for-each))


(define-for-syntax DD-TEMPLATE-RULES
  '(atomic-non-distinct atomic-distinct one-of compound self-ref ref))


(define-syntax (@Problem stx)
  (syntax-case stx ()
    [(_ ns)
     (let ([n (syntax-e #'ns)])
       (unless (and (integer? n)
                    (> n 0))
         (raise-syntax-error "Expected integer 1 or greater." stx #'ns))
       #'(values))]))


(define-syntax (@HtDF stx)
  (syntax-case stx ()
    [(_ id ...)
     (let ([id-stxs (syntax-e #'(id ...))])
       (when (empty? id-stxs)
         (raise-syntax-error #f "expected at least one function name after @HtDF" stx))
       (for ([i id-stxs])
            (unless (symbol? (syntax-e i))
              (raise-syntax-error #f "expected a function name" stx i)))
       #'(values))]))

(define-syntax (@HtDD stx)
  (syntax-case stx ()
    [(_ id ...)
     (let ([id-stxs (syntax-e #'(id ...))])
       (when (empty? id-stxs)
         (raise-syntax-error #f "expected at least one type name after @HtDD" stx))
       (for ([i id-stxs])
            (unless (symbol? (syntax-e i))
              (raise-syntax-error #f "expected a type name" stx i)))
       #'(values))]))

(define-syntax (@HtDW stx)
  (syntax-case stx ()
    [(_ id ...)
     (let ([ids (syntax-e #'(id ...))])
       (unless (and (not (empty? ids))
                    (empty? (rest ids))
                    (symbol? (syntax-e (first ids))))
         (raise-syntax-error #f "expected a type name (the world state for this HtDW design)" stx))
       #'(values))]))


(define-syntax (@dd-template-rules stx)  
  (syntax-case stx ()
    [(_ r ...)
     (for ([rule (syntax-e #'(r ...))])
          (unless (member (syntax-e rule) DD-TEMPLATE-RULES)
            (raise-syntax-error #f
                                (format "Expected one of ~s." DD-TEMPLATE-RULES)
                                stx
                                rule)))
     #'(values)]))

(define-syntax (@template stx)  
  (syntax-case stx ()
    [(_ o ...)
     (for ([origin (syntax-e #'(o ...))])
          (unless (symbol? (syntax-e origin))
            (raise-syntax-error #f
                                (format "Expected a TypeName or one of ~s." TEMPLATE-ORIGINS)
                                stx
                                origin)))
     #'(values)]))



