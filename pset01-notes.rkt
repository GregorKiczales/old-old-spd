;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pset01-notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require "tags.rkt")

(check-expect (+ 1 2) 3)

(@Problem 1)

(+ (* 5 6)
   (/ (* 4 12)
      8))








(@Problem 2)


(define (foo x) x)

;(foo (* 3 5))
;(foo 15)
;(circle (if (and (<= 0 15) (<= 15 MAX)) 15 MAX)
;        "solid"
;        "red")
;(circle (if (and #true (<= 15 MAX)) 15 MAX) "solid" "red")
;(circle (if (and #true #true) 15 MAX) "solid" "red")
;(circle (if true 15 MAX) "solid" "red")
;(circle 15 "solid" "red")








(@Problem 3)

;(substring "Systematic Program Design" 8 "18")














(@Problem 4)

(@HtDF fraction)
;; Number Number -> Number
;; produce result of dividing smaller number by larger
(check-expect (fraction 3 4) .75)
(check-expect (fraction 4 3) .75)
(check-expect (fraction 2 2) 1)

;(define (fraction x y) 0) ;stub

(@template Number add-param)
;(define (fraction x y)    ;template
;  (... x y))


(define (fraction x y)
  (if (< x y)
      (/ x y)
      (/ y x)))

; here are two very good alternative function definitions
;(define (fraction x y)
;  (/ (min x y)
;     (max x y)))
;
;
;(define (fraction x y)
;  (/ (if (< x y) x y)
;     (if (> x y) x y)))



(@HtDD Foo Bar)

(@HtDW Foo)

(@dd-template-rules one-of)

(@template type)
