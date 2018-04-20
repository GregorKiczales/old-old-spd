#lang racket
(require (for-syntax racket/base syntax/parse racket/list))
(require syntax/keyword)
(require htdp/testing (for-syntax htdp/testing))

;; Syntax for @tags including basic syntax checking.
;; The tags are further checked through the grader framework.

(provide @Problem

         @HtDW
         @HtDD
         @HtDF
          
         @dd-template-rules
         @template)


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


(define-syntax (define-@Tag-syntax stx)
  (syntax-case stx ()
    [(_ tag arity-string kind-string checker)
     #`(define-syntax (tag stx)
         
         (unless (member (syntax-local-context) '(module top-level))            
           (raise-syntax-error #f (format "Found ~a that is not at top level" 'tag) stx))
         
         (syntax-case stx ()
           [(_)        (raise-syntax-error #f (format "expected ~a ~a after ~a" arity-string kind-string 'tag) stx)]
           [(_ . id) #'(#%expression (checker . id))]
           [_          (raise-syntax-error #f (format "expected an open parenthesis before ~a" 'tag) stx)]))]))
                                             



(define-@Tag-syntax @Problem                    "one" "integer greater than 0"    check-problem-number)
(define-@Tag-syntax @HtDF              "at least one" "function name"             check-defined-functions)
(define-@Tag-syntax @HtDD              "at least one" "type name"                 check-defined-types)
(define-@Tag-syntax @HtDW                       "one" "type name"                 check-defined-types) ;!!! restrict arity[
(define-@Tag-syntax @template          "at least one" "template origin"           check-template-origins)
(define-@Tag-syntax @dd-template-rules "at least one" "data driven template rule" check-dd-template-rules)


(define-syntax (check-problem-number stx) ;!!! later make this check for duplicates  
  (syntax-case stx ()
    [(_ ns)
     (let ([n (syntax-e #'ns)])
       (unless (and (integer? n)
                    (> n 0))
         (raise-syntax-error "Expected integer 1 or greater." stx #'ns))
       #'(values))]))

(define-syntax (check-defined-functions stx)    
  (syntax-case stx ()
    [(_ i ...)
     (for ([id-stx (syntax-e #'(i ...))])
          (let ([id (syntax-e id-stx)])
            (cond [(not (symbol? id))
                   (raise-syntax-error '@HtDF (format "~a is not a function name" id) stx id-stx)]
                  [(not (identifier-binding id-stx 0 #t))
                   (raise-syntax-error id "this function is not defined" stx id-stx)])))

     #'(values)]))

(define-syntax (check-defined-types stx)    
  (syntax-case stx ()
    [(_ i ...)
     (for ([id-stx (syntax-e #'(i ...))])
          (let ([id (syntax-e id-stx)])
            (cond [(not (symbol? id))
                   (raise-syntax-error '@HtDD (format "~a is not a type name" id) stx id-stx)]
                  ;[(not (identifier-binding id-stx 0 #t))
                  ; (raise-syntax-error id "this function is not defined" stx id-stx)]
                  )))

     #'(values)]))

(define-syntax (check-template-origins stx)    
  (syntax-case stx ()
    [(_ i ...)
     (for ([id-stx (syntax-e #'(i ...))])
          (let ([id (syntax-e id-stx)])
            (cond [(not (symbol? id))
                   (raise-syntax-error '@template (format "~a should be a TypeName or one of ~s." id TEMPLATE-ORIGINS) stx id-stx)]
                  ;[(not (identifier-binding id-stx 0 #t))
                  ; (raise-syntax-error id "this function is not defined" stx id-stx)]
                  )))

     #'(values)]))     

(define-syntax (check-dd-template-rules stx)    
  (syntax-case stx ()
    [(_ i ...)
     (for ([id-stx (syntax-e #'(i ...))])
          (let ([id (syntax-e id-stx)])
            (cond [(not (member id DD-TEMPLATE-RULES))
                   (raise-syntax-error '@dd-template-rules (format "~a is not one of ~a" id (format-list DD-TEMPLATE-RULES #t)) stx id-stx)])))

     #'(values)]))


(define-for-syntax (format-list l or?)
  (cond [(empty? (rest l))
         (format "~a ~a" (if or? "or" "and") (first l))]
        [else
         (format "~a, ~a" (first l) (format-list (rest l) or?))]))



