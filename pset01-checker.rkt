#lang racket

(require "tags.rkt")
(require "grader-utils.rkt")


(define SUBM (parse-submission "pset01-notes.rkt"))

(define P1 (get-problem 1 SUBM)) 

;; !!! I changed these slighty
(define (grade-add n) (fprintf (current-output-port)
                               (string-append "Add " (~v n) " to grade." "\n")))
(define (grade-sub n) (fprintf (current-output-port)
                               (string-append "Subtract " (~v n) " from grade." "\n")))

;;---------------------------------------

(define (message s)
  (fprintf (current-output-port) (string-append s "\n")))

;;---------------------------------------
;;---------------------------------------

(define P1-DEFNS (first (problem-elts P1)))

(define (check-P1)
  (if (equal? P1-DEFNS
              '(+ (* 5 6) (/ (* 4 12) 8)))
      (message "10 points for Problem 1")
      (message "0 points for Problem 1")))

(check-P1)


;;---------------------------------------
(message "")
;;---------------------------------------

(define P3 (get-problem 3 SUBM))

(define P3-RAW (problem-elts P3))

(define P3-ANS
  (remove ";(substring \"Systematic Program Design\" 8 \"18\")"
          (filter (compose not (curry equal? "")) P3-RAW)))



(define (check-P3)
  (cond [(not (= (length P3-ANS) 1))
         (message "0 points: exactly 1 answer not given for Problem 3")]
        [else
         (local [(define line (first P3-ANS))]
           (cond
             [(equal? line
                      '(substring "Systematic Program Design" 8 18))
              (message "5 points for Problem 3")]
             [(equal? line
                      '(substring "Systematic Program Design" 11 18))
              (message "10 points for Problem 3")]
             [else
              (message "0 points for Problem 3")]))]))

(check-P3)
                                                    
;;---------------------------------------
(message "")
;;---------------------------------------

(define P4 (get-problem 4 SUBM))

(define P4-1 (first (problem-elts P4)))

(define P4-SIG (first (htdf-sigs P4-1)))
(define P4-SIG-PARAMS (signature-args P4-SIG))
(define P4-SIG-RESULT (signature-result P4-SIG))

(define P4-PURPOSE (htdf-purposes P4-1))

(define P4-CES (htdf-ces P4-1))

(define P4-TEMPLATE (htdf-template P4-1))
(define P4-TEMPLATE-ORIGINS (template-origins P4-TEMPLATE))

(define P4-DEFNS (htdf-defns P4-1))

;;----------------------------------------

;(define (check-signature ans sol)
;  (let ([nargs (,,,)]
;        [result ...])
;    (when (not (= (length ... nargs (length (signature-args sol)))
;
;
; 
;(check-signature P4-SIGNATURE (make-signature ...))

(define (check-sigs)
  (begin
    (cond [(not (= (length P4-SIG-PARAMS) 2))
           (message "Incorrect number of sig parameters")]
          [(and (equal? (first P4-SIG-PARAMS) 'Number)
                (equal? (second P4-SIG-PARAMS) 'Number))
           (message "Correct sig parameters")]
          [(xor (not (equal? (first P4-SIG-PARAMS) 'Number))
                (not (equal? (second P4-SIG-PARAMS) 'Number)))
           (message "One sig parameter has incorrect type")]
          [else
           (message "Both sig parameters have incorrect types")])
    
    (cond [(equal? P4-SIG-RESULT 'Number)
           (message "Correct sig result")]
          [else
           (message "Incorrect sig result")])))

;;-----------------------------------------

(define (check-template)
  (cond [(= (length P4-TEMPLATE-ORIGINS) 1)
         (if (equal? (first P4-TEMPLATE-ORIGINS) 'atomic)
             (message "Correct template origin")
             (message "Incorrect template origin type"))]
        [(= (length P4-TEMPLATE-ORIGINS) 2)
         (cond [(and (equal? (first P4-TEMPLATE-ORIGINS) 'atomic)
                     (equal? (second P4-TEMPLATE-ORIGINS) 'atomic))
                (message "Correct template origin")]
               [(xor (not (equal? (first P4-TEMPLATE-ORIGINS) 'atomic))
                     (not (equal? (second P4-TEMPLATE-ORIGINS) 'atomic)))
                (message "One template origin has incorrect type")]
               [else
                (message "Both template origins have incorrect types")])]
        [else
         (message "Incorrect number of template origins")]))

;;------------------------------------------

(define ns (make-base-namespace))

(define THEIR-FN (third (first P4-DEFNS)))
(define OUR-FN
  '(if (< x y)
       (/ x y)
       (/ y x)))

(define (eval-fn fn x-val y-val)
  (eval `(let ([x ,x-val]
               [y ,y-val])
           ,fn) ns))

;; Gregor to figure out the "right" way to evaluate c-es.
(define (check-ces)
  (local [(define (check-ces--ce ce)
            (local [(define params (second ce))
                    (define result (third ce))]
              (cond [(not (equal? (eval-fn OUR-FN (second params) (third params))
                                  result))
                     (message "Parameters from a c-e doesn't lead to correct result")])))
          (define (check-ces--loce loce)
            (cond [(empty? loce) (void)]
                  [else
                   (begin
                     (check-ces--ce (first loce))
                     (check-ces--loce (rest loce)))]))]
    (check-ces--loce P4-CES)))

;;-----------------------------------------

(define LISTOF-VALS (list '(1 2) '(3 2) '(43545 56676575)))

(define (check-defns)
  (local [(define (check-defns--val val)
            (local [(define x (first val))
                    (define y (second val))]
              (cond [(not (equal? (eval-fn THEIR-FN x y)
                                  (eval-fn OUR-FN x y)))
                     (message "An x,y pair doesn't produce correct result")])))
          (define (check-defns--lov lov)
            (cond [(empty? lov) (void)]
                  [else
                   (begin
                     (check-defns--val (first lov))
                     (check-defns--lov (rest lov)))]))]
    (check-defns--lov LISTOF-VALS)))

;;------------------------------------------


(define (check-P4)
  (begin
    (check-sigs)
    (check-ces)
    (check-template)
    (check-defns)))

(check-P4)


