#lang racket
(define x (- ( /(+ 9 6) (* (- 3 1) 5)) (* (- 7/8 2) 4)))
x

( define (g u v w)
   (+ (/ (- v (* 7 u)) (- u w) (/ (+ u v) (- (* w 6) v )))))
(g 1 2 3)
(g 3 11 2)

(define (my-max x y)
  (if (> x y) x y))
(my-max 5 2)
(my-max 10 23)
(my-max 4 4)

(define (groesser-zehn? x)
  (> x 10))
(groesser-zehn? 4)
(groesser-zehn? 10)
(groesser-zehn? 15)

(define (groesserp? x y z)
  (> (+ x y) z))
(groesserp? 4 5 6)
(groesserp? 2 12 10)
(groesserp? 3 3 6)

(define (squared-max x y z)
    (max(* x x) (* y y) (* z z)))
(squared-max 2 -3 5)