#lang racket
(require (for-syntax racket/base racket/list racket/snip))
(require racket/function racket/snip)
(require wxme)
(require "tags.rkt")

(provide (all-defined-out)) ;!!!


;; Self-named design structure metadata. Most fields are id, (listof id), or boolean.
;; problem-elts produce listof htdf, htdd, or htdw.

(struct file      (elts))

(struct problem   (num elts)                                   #:transparent #:mutable)
(struct htdf      (names sigs purposes checks templates defns) #:transparent #:mutable)
(struct htdd      (names constants rules templates)            #:transparent #:mutable)
(struct htdw      (ws constants)                               #:transparent #:mutable)
(struct signature (args result fail?)                          #:transparent #:mutable)
(struct template  (origins)                                    #:transparent #:mutable)
(struct dd-template-rules (rules)                              #:transparent #:mutable)



(define (get-problem n f)
  (or (scan-file (lambda (t) (and (problem? t) (= n (problem-num t)))) f)
      (error 'get-problem "No problem number ~v." n)))

(define (get-htdf n f)
  (or (scan-file (lambda (t) (and (htdf? t) (member n (htdf-names t)))) f)
      (error 'get-problem "No htdf for function named ~v." n)))

(define (get-htdd n f)
  (or (scan-file (lambda (t) (and (htdd? t) (member n (htdd-names t)))) f)
      (error 'get-problem "No htdd for type named ~v." n)))

(define (get-htdw n f)
  (or (scan-file (lambda (t) (and (htdw? t) (eqv? n (htdw-ws t)))) f)
      (error 'get-problem "No htdd for type named ~v." n)))

(define (scan-file p f)
  (let loop ([elts (file-elts f)])
    (cond [(empty? elts) #f]
          [(p (first elts)) (first elts)]
          [else
           (loop (rest elts))])))


(define-syntax (define-syntax-predicate stx)
  (syntax-case stx ()
    ((_ p (t ...))
     #'(define (p x)
         (and (syntax? x)
              (let ([e (syntax-e x)])
                (and (list? e)
                     (not (empty? e))
                     (member (syntax-e (first e)) '(t ...)))))))))

(define-syntax-predicate is-at-problem?           (@Problem))
(define-syntax-predicate is-at-htdf?              (@HtDF))
(define-syntax-predicate is-at-htdd?              (@HtDD))
(define-syntax-predicate is-at-htdw?              (@HtDW))
(define-syntax-predicate is-at-signature?         (@signature))
(define-syntax-predicate is-at-template?          (@template))
(define-syntax-predicate is-at-dd-template-rules? (@dd-template-rules))

(define-syntax-predicate is-check?
  (check-expect check-random check-satisfied check-within check-error check-member-of check-range))

(define-syntax-predicate is-define? (define))

(define (is-function-definition? x)
  (and (is-define? x)
       (pair? (syntax-e (cadr (syntax-e x))))))

(define (is-constant-definition? x)
  (and (is-define? x)
       (not (pair? (syntax-e (cadr (syntax-e x)))))))



;; -> File
(define (parse-file fn)
  (with-input-from-file fn
    (lambda ()
      (parse-port (current-input-port)))))

(define (parse-port p)
  (file (parse-elts (read-top-level p))))

;; -> (listof Syntax)
(define (read-top-level p)
  (rest
   (syntax-e
    (fourth
     (syntax-e
      (parameterize ([current-input-port p]  ;(ensure-text-port  ;!!!???
                     [read-accept-reader #t])
        (read-syntax)))))))




(define (parse-elts lostx)
  (parameterize ([all-elements    '()])
    (for ([stx lostx])
         (cond [(is-at-problem? stx)           (parse-problem stx)]
               
               [(is-at-htdf? stx)              (parse-htdf stx)]
               [(is-at-htdd? stx)              (parse-htdd stx)]
               [(is-at-htdw? stx)              (parse-htdw stx)]

               [(is-at-signature? stx)         (parse-signature stx)]
               [(is-at-template? stx)          (parse-template stx)]
               [(is-at-dd-template-rules? stx) (parse-dd-template-rules stx)]
               
               [(is-check? stx)                (parse-check stx)]
               [(is-function-definition? stx)  (parse-fn-defn stx)]
               [(is-constant-definition? stx)  (parse-const-defn stx)]
  
               [else                           (void)]))

    (reverse (all-elements))))


(define all-elements (make-parameter empty)) 

(define (add-elt t)
  (all-elements (cons t (all-elements))))

(define (first-elt p)
  (let loop ([elts (all-elements)])
    (cond [(empty? elts) #f]
          [else
           (if (p (first elts))
               (first elts)
               (loop (rest elts)))])))

(define (rcons lox x) (append lox (list x)))



(define (parse-problem tag)
  (let* ([n       (cadr (syntax->datum tag))]
         [elt     (problem n '())])
    (add-elt elt)))

(define (parse-htdf t)
  (let* ([names   (rest (syntax->datum t))]
         [elt     (htdf names '() '() '() '() '())]
         [pblm    (first-elt problem?)])
    (add-elt elt)
    (when pblm
      (set-problem-elts! pblm (rcons (problem-elts pblm) elt)))))

(define (parse-htdd t)
  (let* ([names   (rest (syntax->datum t))]
         [elt     (htdd names '() '() '())]
         [pblm    (first-elt problem?)])
    (add-elt elt)
    (when pblm
      (set-problem-elts! pblm (rcons (problem-elts pblm) elt)))))

(define (parse-htdw t)
  (let* ([name    (cadr (syntax->datum t))]
         [elt     (htdw name '())]
         [pblm    (first-elt problem?)])
    (add-elt elt)
    (when pblm
      (set-problem-elts! pblm (rcons (problem-elts pblm) elt)))))


(define (parse-signature stx)
  (let* ([types   (rest (syntax->datum stx))]
         [args    (reverse (cdr (member '-> (reverse types))))]
         [result  (cadr (member '-> types))]
         [fail?   (equal? (take (reverse types) 2) '(false or))]
         [elt     (signature args result fail?)]
         [htdf    (first-elt htdf?)])
    (add-elt elt)
    (when htdf
      (set-htdf-sigs! htdf (rcons (htdf-sigs htdf) elt)))))

(define (parse-template stx)
  (let* ([origins (rest (syntax->datum stx))]
         [elt     (template origins)]
         [htdf    (first-elt htdf?)])
    (add-elt elt)
    (when htdf
      (set-htdf-templates! htdf (rcons (htdf-templates htdf) elt)))))

(define (parse-dd-template-rules stx)
  (let* ([rules   (rest (syntax->datum stx))]
         [elt     (dd-template-rules rules)]
         [htdd    (first-elt htdd?)])
    (add-elt elt)
    (when htdf
      (set-htdd-rules! htdd (rcons (htdd-rules htdd) elt)))))



(define (parse-check stx)
  (let ([exp (syntax->datum stx)])
    (add-elt exp)
    (when (and (pair? (cadr exp))
               (symbol? (caadr exp)))
      (let ([htdf (first-elt (lambda (e)
                               (and (htdf? e)
                                    (member (caadr exp) (htdf-names e)))))])
        (when htdf
          (set-htdf-checks! htdf (rcons (htdf-checks htdf) exp)))))))


(define (parse-fn-defn stx)
  (let* ([elt (syntax->datum stx)]
         [name (caadr elt)]
         [sname (symbol->string name)])
    
    (add-elt elt)
    
    (cond [(regexp-match #rx"fn-for-.*" sname)
           (let* ([tname (substring sname (string-length "fn-for-"))]
                  [htdd  (first-elt (lambda (e)
                                      (and (htdd? e)
                                           (ormap (lambda (n)
                                                    (string=? tname
                                                              (string-downcase (symbol->string n))))
                                                  (htdd-names e)))))])
             
             (when htdd
               (set-htdd-templates! htdd (rcons (htdd-templates htdd) elt))))]
          [else
           (let ([htdf (first-elt (lambda (e)
                                    (and (htdf? e)
                                         (member name (htdf-names e)))))])
             (when htdf
               (set-htdf-defns! htdf (rcons (htdf-defns htdf) elt))))])))


(define (parse-const-defn stx)
  (let ([elt (syntax->datum stx)])
    
    (add-elt elt)

    ;; !!! do we add it to any specific elts? HtDD? HtDW? ???
    ))



(define foo (parse-file "example-w-tags.rkt"))


