#lang racket/base

(require racket/syntax
         (for-syntax racket/base
                     syntax/parse
                     stepper/private/syntax-property
                     "constants.rkt")
         (rename-in lang/htdp-intermediate-lambda [local htdp.local])
         "constants.rkt")

;; Syntax for @tags.
;;
;; @tags are checked for well-formedness and some reference checks during
;; syntax expansion, they expand to just (void) so that they don't
;; affect what values are printed when student code is run.
;; 
;; The file structure parser pulls apart the syntax later, knowing the
;; tags are are well-formed.

(provide @TAGS
         NON-TYPE-TEMPLATE-ORIGINS	 

         @assignment
         @cwl
	 
         @problem 

         @htdw      
         @htdd      
         @htdf

         @signature
          
         @dd-template-rules 
         @template-origin
         @template

         take-while
         drop-while

         local)


(define-for-syntax TAGS '())     ;(listof Syntax)  built in expansion phase 0
(define-for-syntax PROBLEMS '()) ;(listof Natural) built in expansion phase 1

(define-for-syntax LOCAL-DEFINES (make-parameter '()))

;; first expansion phase happens here and adds tag to TAGS
;; this allows check-type to find @htdd that occur after references to the type
(define-syntax (define-@tag-syntax stx)
  (syntax-case stx ()
    [(_ tag arity-string kind-string expander)
     #`(define-syntax (tag stx)                 
         (syntax-case stx ()
           [(_)       
            (raise-syntax-error #f (format "expected ~a ~a after ~a" arity-string kind-string 'tag) stx)]
           [(_ . id)   
            (set! TAGS (cons stx TAGS)) 
            (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                             ['stepper-hide-reduction #t])
              #'(#%expression (expander . id)))]
           [_ 
            (raise-syntax-error #f (format "expected an open parenthesis before ~a" 'tag) stx)]))]))
                                             

(define-@tag-syntax @assignment                 "one" "assignment id"             expand-assignment)
(define-@tag-syntax @cwl                 "one or two" "campus wide login"         expand-cwl)
(define-@tag-syntax @problem                    "one" "integer greater than 0"    expand-problem)
(define-@tag-syntax @htdf              "at least one" "function name"             expand-htdf)
(define-@tag-syntax @htdd              "at least one" "type name"                 expand-htdd)
(define-@tag-syntax @htdw                       "one" "type name"                 expand-htdw) 
(define-@tag-syntax @signature         "at least one" "type name"                 expand-signature)
(define-@tag-syntax @template-origin   "at least one" "template origin"           expand-template-origin)
(define-@tag-syntax @template                   "one" "function template"         expand-template)
(define-@tag-syntax @dd-template-rules "at least one" "data driven template rule" expand-dd-template-rules)


(define-for-syntax STEPPER-VOID    
  (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                   ['stepper-hide-reduction #t])
    #'(void)))


(define-syntax (expand-assignment        stx) (check-assignment stx)        STEPPER-VOID)
(define-syntax (expand-cwl               stx) (check-cwl stx)               STEPPER-VOID)
(define-syntax (expand-problem           stx) (check-problem stx)           STEPPER-VOID)
(define-syntax (expand-htdf              stx) (check-htdf stx)              STEPPER-VOID)
(define-syntax (expand-htdd              stx) (check-htdd stx)              STEPPER-VOID)
(define-syntax (expand-htdw              stx) (check-htdw stx)              STEPPER-VOID)
(define-syntax (expand-signature         stx) (check-signature stx)         STEPPER-VOID)
(define-syntax (expand-template-origin   stx) (check-template-origin stx)   STEPPER-VOID)
(define-syntax (expand-template          stx) (check-template stx)          STEPPER-VOID)
(define-syntax (expand-dd-template-rules stx) (check-dd-template-rules stx) STEPPER-VOID)


(define-for-syntax (check-tag-for-local stx)
  (let* ([d (syntax->datum stx)]
         [t (if (pair? d) (car d) d)])
    (case t
      ((@htdf)              (check-htdf stx))
      ((@signature)         (check-signature stx))
      ((@template-origin)   (check-template-origin stx))
      ((@template)          (check-template stx))

      ((@htdd)              (check-htdd stx))
      ((@dd-template-rules) (check-dd-template-rules stx))
      [else
       (raise-syntax-error t (format "found ~a tag that is not at top level" t) stx)])))

(define-syntax (local stx)
  (syntax-case stx ()
    [(_ bindings body)
     (let* ([tags  (filter @tag? (syntax->list #'bindings))]
            [defns (filter not-@tag? (syntax->list #'bindings))]
            [fn-names (map defn-name defns)])
       (set! TAGS (append tags TAGS)) ;ugh, they go to top-level
       (parameterize ([LOCAL-DEFINES fn-names]) ;appending won't work anyways
         (for ([tag tags])
           (check-tag-for-local tag)))
       #`(htdp.local #,(filter not-@tag? (syntax->list #'bindings)) body))]))




(define-for-syntax (check-htdf stx)
  (syntax-case stx ()
    [(_ id-stx ...)
     (let ([id-stxs (syntax-e      #'(id-stx ...))]
           [ids     (syntax->datum #'(id-stx ...))])
       (for ([id-stx id-stxs]
             [id     ids])
         (cond [(not (symbol? id))
                (raise-syntax-error '@htdf (format "~a is not a well-formed function name" id) stx id-stx)]
               [(not (or (identifier-binding id-stx 0 #t)
                         (memq id (LOCAL-DEFINES))))
                (raise-syntax-error id "Cannot find define for this function name" stx id-stx)])))]))

(define-for-syntax (check-htdd stx)    
  (syntax-case stx ()
    [(_ i ...)
     (for ([id-stx (syntax-e #'(i ...))])
       (let ([id (syntax-e id-stx)])
;         (cond [(member id PRIMITIVE-TYPES)
;                (raise-syntax-error '@htdd (format "Must not redefine the primitive type ~a" id) stx id-stx)]
          (when (not (and (symbol? id)
                          (looks-like-type-name? id)))
            (raise-syntax-error '@htdd (format "~a is not a well formed type name" id) stx id-stx))))
     (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                      ['stepper-hide-reduction #t])
       #'(void))]))


(define-for-syntax (check-assignment stx)
  (syntax-case stx ()
    [(_ id) 
     (unless (symbol? (syntax-e #'id))
       (raise-syntax-error '@assignment (format "~s has the wrong form for an assignment id" (syntax->datum #'id)) stx #'id))
     (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                      ['stepper-hide-reduction #t])
       #'(void))]))

(define-for-syntax (check-cwl stx)
  (syntax-case stx ()
    [(_ id1)
     (unless (symbol? (syntax-e #'id1))
       (raise-syntax-error '@cwl (format "~a has the wrong form for a CWL" (syntax->datum #'id1)) stx #'id1))]
    [(_ id1 id2) 
     (let ()
       (unless (symbol? (syntax-e #'id1))
         (raise-syntax-error '@cwl (format "~a has the wrong form for a CWL" (syntax->datum #'id1)) stx #'id1))
       (unless (symbol? (syntax-e #'id2))
         (raise-syntax-error '@cwl (format "~a has the wrong form for a CWL" (syntax->datum #'id2)) stx #'id2))
       (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                        ['stepper-hide-reduction #t])
         #'(void)))]))


(define-for-syntax (check-problem stx)
  (syntax-case stx ()
    [(_ ns)
     (let ([n (syntax-e #'ns)])
       (cond [(null? PROBLEMS)
              (unless (equal? n 1)
                (raise-syntax-error '@problem "expected 1 (first problem number must be 1)." stx #'ns))]
             [(not (= n (add1 (car PROBLEMS))))
              (raise-syntax-error '@problem
                                  (format "previous @problem tag is number ~a, expected this one to be number ~a"
                                          (car PROBLEMS)
                                          (add1 (car PROBLEMS)))
                                  stx
                                  #'ns)])
       (set! PROBLEMS (cons n PROBLEMS))
       (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                        ['stepper-hide-reduction #t])
         #'(void)))]))


(define-for-syntax (check-signature stx)
  (syntax-parse stx #:datum-literals (-> or false)
    [(_ arg ... -> result or false)
     (check-types '@signature (syntax-e #'(arg ...)) (syntax->datum #'(arg ...)))
     (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                      ['stepper-hide-reduction #t])
       #'(void))]
    [(_ arg ... -> result)
     (check-types '@signature (syntax-e #'(arg ...)) (syntax->datum #'(arg ...)))
     (with-stepper-syntax-properties (['stepper-skip-completely #t]
                                      ['stepper-hide-reduction #t])
       #'(void))]))


(define-for-syntax (check-template-origin stx)    
  (syntax-case stx ()
    [(_ t ...)
     (let* ([t-stxs (syntax-e #'(t ...))]
            [ts     (map syntax->datum t-stxs)]
            [seen   '()])
       (for ([stx  t-stxs]
             [type ts])
         (cond [(looks-like-type-name? type) (check-type '@template-origin stx type)]
               ;[(looks-like-type-param? type) (check-type '@template-origin stx type)]
               [(symbol? type)
                (when (member type seen)
                  (raise-syntax-error '@template-origin
                                      (format "~a is not allowed to be used twice" type) stx stx))
                (when (not (member type NON-TYPE-TEMPLATE-ORIGINS))
                  (raise-syntax-error '@template-origin
                                      (format "~a is not a legal type name, (listof <TypeName>), or one of ~s" type (format-list NON-TYPE-TEMPLATE-ORIGINS #t)) stx stx))
                (when (and (member type '(bin-tree arb-tree))
                           (not (member 'genrec ts)))
                  (raise-syntax-error '@template-origin
                                      (format "using ~a requires also using genrec" type) stx stx))
                (set! seen (cons type seen))]
               [else
                (raise-syntax-error '@template-origin (format "~a should be a TypeName or one of ~s." type NON-TYPE-TEMPLATE-ORIGINS) stx stx)])))]))

(define-for-syntax (check-template stx)
  (syntax-case stx ()
    [(_ defn)
     (let ([datum (syntax->datum #'defn)])
       (unless (and (list? datum)
                    (= (length datum) 3)
                    (eqv? (car datum) 'define)
                    (pair? (cadr datum))
                    (> (length (cadr datum)) 1))
         (raise-syntax-error '@template (format "~a is not a well-formed function definition template" datum) stx #'defn)))]))

(define-for-syntax (check-htdw stx)    
  (syntax-case stx ()
    [(_ stx)
     (let ([type (syntax->datum #'stx)])
       (cond [(looks-like-type-name? type)
              (check-type '@htdw #'stx type)]
             [else
              (raise-syntax-error '@htdw (format "~a is not a type name or (listof TypeName)" type) #'stx #'stx)]))]))


(define-for-syntax (check-dd-template-rules stx)    
  (syntax-case stx ()
    [(_ i ...)
     (for ([id-stx (syntax-e #'(i ...))])
       (let ([id (syntax-e id-stx)])
         (cond [(not (member id DD-TEMPLATE-RULES))
                (raise-syntax-error '@dd-template-rules (format "~a is not one of ~a" id (format-list DD-TEMPLATE-RULES #t)) stx id-stx)])))]))


(define-for-syntax (check-types who stxs types)
  (for ([stx stxs] [type types]) (check-type who stx type)))

(define-for-syntax (check-type who stx type)
  (cond ((and (list? type)
              (= (length type) 2)
              (eqv? (car type) 'listof))
         (check-type who stx (cadr type)))
        ((and (list? type)
              (member '-> type)
              (list (cdr (member '-> type))))
         (and (andmap (lambda (t)
                        (check-type who stx t))
                      (reverse (cdr (member '-> (reverse type)))))
              (check-type who stx (cadr (member '-> type)))))
        (else
         (when (and (not (member type PRIMITIVE-TYPES))
                    (not (lookup-htdd type))
                    (not (and (symbol? type)
                              (= (string-length (symbol->string type)) 1))))
	       
           (raise-syntax-error who
                               (format "~a is not a primitive type, cannot find an @htdd tag for it, and it is not a type parameter" type)
                               stx stx)))))

(define-for-syntax (lookup-htdd id) 
  (let loop ([tags TAGS])
    (if (null? tags)
        #f
        (let* ([t (car tags)]
               [d (syntax->datum t)])
          (if (and (eqv? (car d) '@htdd)
                   (member id (cdr d)))
              t
              (loop (cdr tags)))))))

(define-for-syntax (format-list l or?)
  (cond [(null? (cdr l))
         (format "~a ~a" (if or? "or" "and") (car l))] 
        [else
         (format "~a, ~a" (car l) (format-list (cdr l) or?))]))

(define-for-syntax (looks-like-type-name? x)
  (or (and (symbol? x)
           (char-upper-case? (string-ref (symbol->string x) 0)))
      (and (list? x)
           (= (length x) 2)
           (eqv? (car x) 'listof))))


(define-for-syntax (@tag? x)
  (syntax-case x ()
    [(tag . rest) (memq (syntax-e #'tag) @TAGS)]))

(define-for-syntax (not-@tag? x)
  (syntax-case x ()
    [(tag . rest) (not (memq (syntax-e #'tag) @TAGS))]))


(define-for-syntax (defn-name stx)
  (syntax-case stx ()
    [(define (fn-name . id) body) (syntax-e #'fn-name)]
    [(define var-name val) #f]))
  



(define (take-while p lox)
  (cond [(null? lox) '()]
        [else
         (if (p (car lox))
             (cons (car lox) (take-while p (cdr lox)))
             '())]))

(define (drop-while p lox)
  (cond [(null? lox) '()]
        [else
         (if (p (car lox))
             (drop-while p (cdr lox))
             lox)]))
