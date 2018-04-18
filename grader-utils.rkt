#lang racket
(require syntax/parse)
(require racket/syntax)
(require (for-syntax racket/base racket/list racket/syntax syntax/parse syntax/free-vars))
(require rackunit)
(require "tags.rkt")
(require (rename-in "read-type.rkt"
                    (function-type signature)
                    (function-type-args signature-args)
                    (function-type-result signature-result)))

(provide signature signature-args signature-result (all-defined-out)) ;!!! trim this




;; Self-named metadata. Most fields are id, (listof id), number, or boolean; but
;; problemt-elt produces an htdf, htdd, or htdw.

(struct file     (contents))

(struct problem  (num elts)                               #:transparent)
(struct htdf     (names sigs purposes ces template defns) #:transparent)
(struct htdd     (name form)                              #:transparent)
(struct htdw     (ws)                                     #:transparent)
(struct template (origins)                                #:transparent)
(struct dd-template-rules (rules)                         #:transparent)

;(struct signature (params result fail?) #:transparent)  comes from read-type


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
(define-syntax-predicate is-at-template?          (@template))
(define-syntax-predicate is-at-dd-template-rules? (@dd-template-rules))

(define-syntax-predicate is-check?
  (check-expect check-random check-satisfied check-within check-error check-member-of check-range))

(define-syntax-predicate is-define? (define))

(make-parameter 'foo)

(define (parse-submission fn)
  (parse-from-sss (ss-stream (read-top-level-expressions-and-comment-lines fn))))

(define (parse-from-sss sss)
  (let loop ([r '()])
    (let ([p (ss-stream-peek sss)])             
      (cond [(eof-object? p)    (file (reverse r))]
            [(string? p)        (loop (cons (ss-stream-pop sss) r))]
            [(is-at-problem? p) (loop (cons (parse-problem sss) r))]
            
            [(is-at-htdf? p) (loop (cons (parse-htdf sss) r))]   ;collect these
            ;[(is-at-htdd? p) (loop (cons (parse-htdd sss) r))]  ;for poorly
            ;[(is-at-htdw? p) (loop (cons (parse-htdw sss) r))]  ;structured file
            
            [else (ss-stream-pop sss) (loop r)]))))


(define (get-problem n f)
  (let loop ([p (file-contents f)])
    (cond [(empty? p) (error 'get-problem "No problem number ~v." n)]
          [(and (problem? (first p))
                (= (problem-num (first p)) n))
           (first p)]
          [else (loop (rest p))])))

;; called when pop of s produces syntax for (@Problem)
(define (parse-problem sss)
  (let* ([tag (ss-stream-pop sss)]
         [n   (syntax-e (second (syntax-e tag)))]) ;we know it's well formed
    (let loop ([elts '()])
      (let ([p (ss-stream-peek sss)])
        (cond [(eof-object? p)    (problem n (reverse elts))]
              [(is-at-problem? p) (problem n (reverse elts))]
              [(string? p)        (loop (cons (ss-stream-pop sss) elts))]
              [(is-at-htdf? p)    (loop (cons (parse-htdf sss) elts))]
              [(is-at-htdd? p)    (loop (cons (parse-htdd sss) elts))]
              [(is-at-htdw? p)    (loop (cons (parse-htdw sss) elts))]
              [else               (loop (cons (syntax->datum (ss-stream-pop sss)) elts))])))))


(define (parse-htdf sss) 
  (let* ([tag      (ss-stream-pop sss)]
         [names    (rest (syntax->datum tag))]
         [sigs     (parse-signatures  names sss)]
         [purposes (parse-purposes    names sss)]
         [ces      (parse-ces         names sss)]
         [templ    (parse-template    names sss)]   ;!!! just one of these?
         [defns    (parse-fn-defines  names sss)])
    
    (htdf names sigs purposes ces templ defns)))

(define (parse-signatures names sss)
  (for/list ([n (in-range (length names))])
    (let ([p (ss-stream-peek sss)])
      (cond [(not (string? p)) (error "Expected " (length names) " signatures.")]
            [(not (string-contains? p "->")) (error "No signature line immediately following @HtDF.")]
            [else
             (read-type-string (clean-signature (ss-stream-pop sss)))
             #;
             (parse-signature
              (syntax->datum
               (with-input-from-string (clean-signature (ss-stream-pop sss))
                 read-syntax)))]))))

(define (clean-signature str)
  (let* ([no-ws    (string-trim str #:repeat? #t)]
         [no-semis (string-trim no-ws ";" #:repeat? #t)]
         [parens   (string-append "(" no-semis ")")])
    parens))

(define (parse-purposes names sss)
  (for/list ([n (in-range (length names))])
    (let ([p  (ss-stream-peek sss)])
      (when (not (string? p))
        (raise-syntax-error 'foo "Too few purpose lines." p))
      (ss-stream-pop sss))))

(define (collect-ces fn-name) 1)
  

(define (parse-ces names sss)
  (local [(define (next)
            (let ([p (ss-stream-peek sss)])
              (cond [(is-check? p)
                     (cons (syntax->datum (ss-stream-pop sss)) (next))] ;better have l->r evaluation
                    [(string? p) (ss-stream-pop sss) (next)]
                    [(syntax? p) empty]
                    [(eof-object? p) empty])))]
    
    (let ([ces (next)])
      (if (empty? ces)
          (warn "No check-expects found.")
          ces))))

(define (parse-template names sss)
  (local [(define (seek)
            (let ([p (ss-stream-peek sss)])
              ;; !!! isn't allowing for blank or comment lines
              (cond [(eof-object? p) (error "Could not find @template for " names)]
                    [(string? p) (ss-stream-pop sss) (seek)]
                    [(is-at-template? p) (template (rest (syntax->datum (ss-stream-pop sss))))]
                    [else
                     ;; works, but off by 3 lines because of header lines!!!
                     (raise-syntax-error 'foo "Expected @template after check-expects for " p)])))]
    (seek)))

(define (parse-fn-defines names sss)
  (cond [(empty? names) empty]
        [else
         (cons (parse-fn-define (first names) sss)
               (parse-fn-defines (rest names) sss))]))

(define (parse-fn-define name sss)
  (let ([p (ss-stream-peek sss)])
    (cond [(eof-object? p) (error "Expected define for " name)]
          [(string? p)     (ss-stream-pop sss) (parse-fn-define name sss)]
          [(is-define? p)
           (let [(d (syntax->datum (ss-stream-pop sss)))]
             (if (and (list? (second d))
                      (eqv? (first (second d)) name))
                 d
                 (begin
                   (error "Expected define for " name ". Found " d)
                   d)))])))

;; aargh! what about HtDDs with mutual ref  the TCs, examples  and examples get mixed
(define (parse-htdd sss) 
  (let* ([tag      (ss-stream-pop sss)]
         [names    (rest (syntax->datum tag))]
         [sigs     (parse-signatures  names sss)]
         [purposes (parse-purposes    names sss)]
         [ces      (parse-ces         names sss)]
         [templ    (parse-template    names sss)]   ;!!! just one of these?
         [defns    (parse-fn-defines  names sss)])
    
    (htdf names sigs purposes ces templ defns)))

(define (parse-htdw sss) (ss-stream-pop sss))
  

(define (warn . x) x)

;make-limited-input-port will be useful when this gets integrated into
;the @HtDF parser
(define (read-ids-from-string str)
  (map (lambda (t) (datum->syntax #f t))
       (with-input-from-string str (lambda () (port->list)))))



  
;make-limited-input-port will be useful when this gets integrated into
  

  


;; A stream of syntax|string that supports peek and pop.
(struct ss-stream (loss) #:mutable)

(define (ss-stream-peek s)
  (let ([loss (ss-stream-loss s)])
    (if (empty? loss)
        eof
        (first loss))))

(define (ss-stream-pop s)
  (let ([loss (ss-stream-loss s)])
    (if (empty? loss)
        eof
        (begin
          (set-ss-stream-loss! s (rest loss))
          (first loss)))))

(define (open-ss-stream f)
  (ss-stream (read-top-level-expressions-and-comment-lines f)))

(define (read-top-level-expressions-and-comment-lines f)
  (add-comment-lines f (read-top-level-expressions f)))

;; -> (listof Syntax)
(define (read-top-level-expressions f)
  (rest
   (syntax-e
    (fourth
     (syntax-e
      (parameterize ([read-accept-reader #t])
        (with-input-from-file f
          (lambda ()
            (port-count-lines! (current-input-port))
            (read-syntax)))))))))


(define (add-comment-lines f loe)
  (with-input-from-file f
    (lambda ()
      (port-count-lines! (current-input-port))                
      (let loop ([loe loe]
                 [pos 0])
        (local [(define (include-string s los)
                  (cond [(string=? s "") los]
                        [else
                         (cons s los)]))]
                  
          (cond [(empty? loe) (string-split (read-string 10000) "\n")] ;!!!
                [else
                 (let* ([e (first loe)]
                        [ep (syntax-position e)] ; 1-based
                        [es (syntax-span e)])

                   (cond [(< pos ep)
                          (append (string-split (read-string (- ep pos 1)) "\n")
                                  (loop loe ep))]
                         [(= pos ep)
                          (read-string (+ es 1))
                          (cons e
                                (loop (rest loe) (+ pos es)))]))]))))))

;







