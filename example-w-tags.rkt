;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname example-w-tags) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;(require spd/tags) ;!!!
;(require 2htdp/universe)
;(require 2htdp/image)



(@Problem 1)       ;from here to next @Problem is problem 1
(@HtDF distance)
(@signature Number Number -> Number)
;; Produce cartesian distance from origin (0,0) to given (x,y).
(check-expect (distance 3 4) 5)
(check-within (distance 1 1) (sqrt 2) .00001)

(@template Number add-param)
(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))


(@Problem 2)
(@HtDW State)

;; Constants

(define STATE-MIN 0)
(define STATE-MAX 10)

(@HtDD State)
;; State is one of:
;;  - "not-started"
;;  - Number           ;see STATE-MIN and STATE-MAX
;; interp. "not-started" until launched,
;;          afterwards the speed in pixels per tick
(define S1 "not-started")
(define S2 3)

(@dd-template-rules one-of atomic-distinct atomic-non-distinct)
(define (fn-for-state s)
  (cond [(string? s) (...)]
        [else
         (... s)]))

(@Problem 3)
(@HtDF main)
(@signature State -> State)
;; main function for game, start with (main "not-started")
;<no tests for main functions>

(@template htdw-main)

(define (main ws)
  (big-bang ws
    (on-tick tock)      ;State -> State
    (to-draw render)))  ;State -> Image

(@HtDF tock)
(@signature State -> State)
;; if started, advance by 1, looping back to STATE-MIN after STATE-MAX
(check-expect (tock "not-started") "not-started")
(check-expect (tock 1) 2)
(check-expect (tock STATE-MIN) (add1 STATE-MIN))
(check-expect (tock STATE-MAX) STATE-MIN)

;(define (tock s) s) ;stub

(@template State)
(define (tock s)
  (cond [(string? s) "not-started"]
        [else
         (if (> (+ s 1) STATE-MAX)
             STATE-MIN
             (+ s 1))]))


(@HtDF render)
(@signature State -> Image)
;; render the state, with press to start message or the number
(check-expect (render "not-started")
              (text "Press space bar to start." 20 "black"))
(check-expect (render 1) (text "1" 20 "black"))

;(define (render s) empty-image)

(@template State)
(define (render s)
  (text (cond [(string? s) "Press space bar to start."]
              [else        (number->string s)])
        20
        "black"))


(@Problem 4)
(@HtDD Tree ListOfTree)
(define-struct node (name subs))
;; Tree is...
;; ListOfTree is ...
(@dd-template-rules compound atomic-non-distinct ref)
(define (fn-for-tree t) ...)

(@dd-template-rules one-of atomic-distinct compound ref self-ref)
(define (fn-for-lot lot) ...)


(define TC (make-node "c" empty))
(define TD (make-node "d" empty))
(define TA (make-node "a" (list (make-node "b" (list TC TD)))))


(@HtDF search--tree)
(@signature Tree String -> Tree or false)
;; backtracking search for node named n
;;!!
(define (search--tree t n) false)

(@HtDF search--lot)
(@signature ListOfTree String -> Tree or false)
;; backtracking search for node named n
;;!!
(define (search--lot lot n) false)




