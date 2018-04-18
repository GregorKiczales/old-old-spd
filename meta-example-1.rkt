;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname meta-example-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "tags.rkt")

(@Problem 1)
(@HtDF distance)
;; Number Number -> Number   
;; Produce cartesian distance from origin (0,0) to given (x,y).
(check-expect (distance 3 4) 5)
(check-within (distance 1 1) (sqrt 2) .00001)

(@template type add-param)
(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))



(@Problem 2)
(@HtDD State)
;; State is one of:
;;  - "not-started"
;;  - Number[0, 10]
;; interp. "not-started" until launched, afterwards the speed
;;         in pixels per tick
(define S1 "stopped")
(define S2 3)

(@dd-template-rules one-of atomic-distinct atomic-non-distinct)
(define (fn-for-state s)
  (cond [(string? s) (...)]
        [else
         (... s)]))
