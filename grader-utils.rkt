#lang racket
(require (for-syntax racket/base racket/list racket/snip))
(require racket/function racket/snip)
(require wxme)
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
(struct htdd     (names)                                  #:transparent)
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


(define tag-context (make-parameter empty)) ;path to current tag
(define all-tags    (make-parameter empty)) ;all tags

(define (record-tag t)
  (begin (all-tags (cons t (all-tags)))
         t))

(define (raise-error msg)
  (raise-syntax-error #f msg (first (tag-context))))

(define (parse-file fn)
  (parse-from-ss-stream
   (ss-stream
    (add-comment-lines fn
                       (read-top-level-expressions fn)))))


(define (parse-from-ss-stream sss)
  (parameterize ([tag-context '()]
                 [all-tags    '()])
    (let loop ()
      (let ([p (ss-stream-peek sss)])             
        (cond [(eof-object? p)    (file (reverse (all-tags)))]
              
              [(string? p)        (ss-stream-pop sss) (loop)]  ; (cons (ss-stream-pop sss) r))]
              
              [(is-at-problem? p) (parse-problem sss) (loop )];(cons (parse-problem sss) r))]
              [(is-at-htdf? p)    (parse-htdf sss)    (loop )];(cons (parse-htdf sss) r))]   
              [(is-at-htdd? p)    (parse-htdd sss)    (loop )];(cons (parse-htdd sss) r))]   
              [(is-at-htdw? p)    (parse-htdw sss)    (loop )];(cons (parse-htdw sss) r))]   
            
              [else               (ss-stream-pop sss) (loop)]))))); r)])))))


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
  (let loop ([lot (file-contents f)])
    (cond [(empty? lot) #f]
          [(p (first lot)) (first lot)]
          [else
           (loop (rest lot))])))


;; called when pop of s produces syntax for (@Problem)
(define (parse-problem sss)
  (let* ([tag (ss-stream-pop sss)]
         [n   (syntax-e (second (syntax-e tag)))]) ;we know it's well formed
    (parameterize ([tag-context (cons tag (tag-context))])
      (let loop ([elts '()])
        (let ([p (ss-stream-peek sss)])
          (cond [(eof-object? p)    (record-tag (problem n (reverse elts)))]
                [(is-at-problem? p) (record-tag (problem n (reverse elts)))]
                [(string? p)        (loop (cons (ss-stream-pop sss) elts))]
                [(is-at-htdf? p)    (loop (cons (parse-htdf sss) elts))]
                [(is-at-htdd? p)    (loop (cons (parse-htdd sss) elts))]
                [(is-at-htdw? p)    (loop (cons (parse-htdw sss) elts))]
                [else               (loop (cons (syntax->datum (ss-stream-pop sss)) elts))]))))))


(define (parse-htdf sss) 
  (let* ([tag      (ss-stream-pop sss)]
         [names    (rest (syntax->datum tag))])
    (parameterize ([tag-context (cons tag (tag-context))])
      (let ([sigs     (parse-signatures  names sss)]
            [purposes (parse-purposes    names sss)]
            [ces      (parse-ces         names sss)]
            [templ    (parse-template    names sss)]   ;!!! just one of these?
            [defns    (parse-fn-defines  names sss)])
    
        (record-tag (htdf names sigs purposes ces templ defns))))))


;; aargh! what about HtDDs with mutual ref  the TCs, examples  and examples get mixed
(define (parse-htdd sss)
  (let* ([tag      (ss-stream-pop sss)]
         [names    (rest (syntax->datum tag))]
         ;        [sigs     (parse-signatures  names sss)]
         ;        [purposes (parse-purposes    names sss)]
         ;        [ces      (parse-ces         names sss)]
         ;        [templ    (parse-template    names sss)]   ;!!! just one of these?
         ;        [defns    (parse-fn-defines  names sss)]
         )
    
    (record-tag (htdd names))))

(define (parse-htdw sss) 
  (let* ([tag      (ss-stream-pop sss)]
         [name     (cadr (syntax->datum tag))])
    
    (record-tag (htdw name))))

(define (parse-signatures names sss)
  (for/list ([n (in-range (length names))])
            (let ([p (ss-stream-peek sss)])
              (cond [(not (string? p)) (error "Expected " (length names) " signatures.")]
                    [(not (string-contains? p "->")) (raise-error "No signature line immediately following @HtDF.")]
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
                (raise-error "Too few purpose lines."));!!! foo
              (ss-stream-pop sss))))
  

(define (parse-ces names sss)
  (local [(define (next)
            (let ([p (ss-stream-peek sss)])
              (cond [(is-check? p)
                     (cons (syntax->datum (ss-stream-pop sss)) (next))] 
                    [(string? p) (ss-stream-pop sss) (next)]
                    [(syntax? p) empty]
                    [(eof-object? p) empty])))]
    
    (next)))

(define (parse-template names sss);!!! has to keep looking for n tags, intermixed w/ fns
  (local [(define (seek)
            (let ([p (ss-stream-peek sss)])
              ;; !!! isn't allowing for blank or comment lines
              (cond [(eof-object? p) (error "Could not find @template for " names)]
                    [(string? p) (ss-stream-pop sss) (seek)]
                    [(is-at-template? p) (template (rest (syntax->datum (ss-stream-pop sss))))]
                    [else
                     ;; works, but off by 3 lines because of header lines!!!
                     (raise-error "Expected @template after check-expects for function design.")])))]
    (seek)))

(define (parse-fn-defines names sss)
  (for/list ([n names])
            (parse-fn-define n sss)))

(define (parse-fn-define name sss)
  (let ([p (ss-stream-peek sss)])
    (cond [(eof-object? p) (raise-error "Expected define for @htdf tag.")]
          [(string? p)     (ss-stream-pop sss) (parse-fn-define name sss)]
          [(is-define? p)
           (let [(d (syntax->datum (ss-stream-pop sss)))]
             (if (and (list? (second d))
                      (eqv? (first (second d)) name))
                 d
                 (begin
                   (error "Expected define for " name ". Found " d)
                   d)))])))

  


;make-limited-input-port will be useful when this gets integrated into
;the @HtDF parser
(define (read-ids-from-string str)
  (map (lambda (t) (datum->syntax #f t))
       (with-input-from-string str (lambda () (port->list)))))



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

#;
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
            (parameterize ([current-input-port (ensure-text-port (current-input-port))])
              ;(port-count-lines! (current-input-port))
              (read-syntax))))))))))

;; File (listof Syntax) -> (listof Syntax|String)
;; intersperse comment LINES between syntax objects, ignores end of line comments
(define (add-comment-lines f los)
  (with-input-from-file f
    (lambda ()
      (parameterize ([current-input-port (ensure-text-port (current-input-port))])
        ;(port-count-lines! (current-input-port)) ;turns on port-next-location q q
        ;; fp is current file position
        ;; sp is position of (first los)
        ;; ss is span     of (first los)

      
        (local [(define (read-lines amt)
                  (if (zero? amt)
                      '() 
                      (let ([raw (read-string amt)])
                        (if (eof-object? raw)
                            '()
                            (string-split raw "\n")))))
                
                (define (eol!)
                  (let ([char (peek-char (current-input-port))])
                    (cond [(eof-object? char) 0]
                          [(eqv? char #\newline) (read-char (current-input-port)) 1]
                          [else
                           (read-char (current-input-port))
                           (add1 (eol!))])))]
          
          (let loop ([los los]
                     [fp 1])               
                  
            (cond [(empty? los) (list (read-line))];(string-split (read-string 10000) "\n")] ;!!!
                  [else
                   (let* ([s (first los)]
                          [sp (syntax-position s)] ; 1-based
                          [ss (syntax-span s)])

                     (cond [(<= fp (sub1 sp))            
                            (append (read-lines (- sp fp 1))
                                    (loop los sp))]
                           [(= fp sp)             
                            (file-position (current-input-port) (+ sp ss -1)) ;jump to end of syntax
                            (let ([skip (eol!)])                              ;and the rest of that line
                              (cons s
                                    (loop (rest los) (+ sp ss skip))))]))])))))))

(define (ensure-text-port p)
  (if (is-wxme-stream? p)
      (input-port->text-input-port p)
      p))


;; ============================================================================
;; Text conversion

;; Code that turns binary stuff into text is split into three places:
;; * input-port->text-input-port implements a simple generic textualization
;;   filter
;; * snip->text is used earlier in the process, where comment-box text is still
;;   available

(require framework ; for drracket snips, used below
         mrlib/matrix-snip) ; avoid errors from files with matrix snips

;; input-port->text-input-port : input-port (any -> any) -> input-port
;;  the `filter' function is applied to special values; the filter result is
;;  `display'ed into the stream in place of the special
(define (input-port->text-input-port src . filter)
  ;; note that snip->text below already takes care of some snips
  (define (item->text x)
    (cond [(is-a? x snip%)
           (format "~a" (or (send x get-text 0 (send x get-count) #t) x))]
          [(special-comment? x)
           (format "#| ~a |#" (special-comment-value x))]
          [(syntax? x) (syntax->datum x)]
          [else x]))
  (let-values ([(filter) (if (pair? filter) (car filter) item->text)]
               [(in out) (make-pipe 4096)])
    (thread
     (lambda ()
       (let ([s (make-bytes 4096)])
         (let loop ()
           (let ([c (read-bytes-avail! s src)])
             (cond [(number? c) (write-bytes s out 0 c) (loop)]
                   [(procedure? c)
                    (let ([v (let-values ([(l col p) (port-next-location src)])
                               (c (object-name src) l col p))])
                      (display (filter v) out))
                    (loop)]
                   [else (close-output-port out)])))))) ; Must be EOF
    in))

(define (snip->text x)
  (let ([name (and (is-a? x snip%)
                   (send (send x get-snipclass) get-classname))])
    (cond [(equal? name "wximage") "{{IMAGE}}"]
          [(regexp-match? #rx"(lib \"comment-snip.(?:rkt|ss)\" \"framework\")"
                          name)
           ;; comments will have ";" prefix on every line, and "\n" suffix
           (format ";{{COMMENT:\n~a;}}\n"
                   (send x get-text 0 (send x get-count)))]
          [else x])))

(define (untabify str)
  (let loop ([idx 0] [pos 0] [strs '()])
    (let ([tab (regexp-match-positions #rx"\t" str idx)])
      (if tab
        (let* ([pos (+ pos (- (caar tab) idx))]
               [newpos (* (add1 (quotient pos 8)) 8)])
          (loop (cdar tab) newpos
                (list* (make-bytes (- newpos pos) 32)
                       (subbytes str idx (caar tab))
                       strs)))
        (apply bytes-append (reverse (cons (subbytes str idx) strs)))))))

(define foo (parse-file "example-w-tags.rkt"))

;(define bar (read-top-level-expressions (first (directory-list "../pset-solutions/" #:build? #t))))




