#lang racket
;Chatgpt Aufgaben

;Aufgabe A1
(define (min-verkettung prozedur-liste argument)
  (define (iter rest ergebnis)
    (if (null? rest)
        (apply min ergebnis)
        (iter (cdr rest) (cons ((car rest) argument) ergebnis))))
  (iter prozedur-liste '()))

(min-verkettung (list abs (lambda (x) (* x x)) (lambda (x) (- x 10))) 5)

;Aufgabe A2
(define (gefilterte-summe prozedur-liste argument praedikat)
  (define (iter rest ergebnis)
    (if (null? rest)
        (apply + ergebnis)
        (if (praedikat ((car rest) argument))
            (iter (cdr rest) (cons ((car rest) argument) ergebnis))
            (iter (cdr rest) ergebnis))))
  (iter prozedur-liste '()))

(gefilterte-summe (list abs (lambda (x) (* x x)) (lambda (x) (- x 3))) -2 even?)

;Augabe A3
(define (max-prozedur prozedur-liste argument)
  (define (iter rest mprozedur hoechste)
    (if (null? rest)
        mprozedur
        (let ([wert ((car rest) argument)])
          (if (> wert hoechste)
              (iter (cdr rest) (car rest) wert)
              (iter (cdr rest) mprozedur hoechste)))))

  (iter (cdr prozedur-liste) (car prozedur-liste) ((car prozedur-liste) argument)))

(max-prozedur (list abs (lambda (x) (* x x)) (lambda (x) (+ x 10))) 3)

(define f
  (max-prozedur
   (list abs (lambda (x) (* x x)) (lambda (x) (+ x 10)))
   3))

(f 3)
;; => 13

;Aufgabe B1
(define (generator-if prozedur start ende praedikat)
  (define (iter counter ls)
    (if (> counter ende)
        (reverse ls)
        (if (praedikat (prozedur counter))
            (iter (+ counter 1) (cons (prozedur counter) ls))
            (iter (+ counter 1) ls))))
  (iter start '()))

(generator-if (lambda (x) (* x x)) 1 6 even?)

;Aufgabe B2
(define (generator-bis prozedur start praedikat)
  (define (iter counter ls)
    (if (not (praedikat (prozedur counter)))
        (reverse ls)
        (iter (+ counter 1) (cons (prozedur counter) ls))))
  (iter start '()))

(generator-bis (lambda (x) (* x 2)) 1 (lambda (x) (< x 10)))

;Aufgabe C1
(define (arbeiten2 liste p1 p2)
  (define (iter old new)
    (if (null? old)
        new
        (cond
          [(and (p1 (car old)) (p2 (car old))) (iter (cdr old) (append new (list (car old) (car old) (car old))))]
          [(p2 (car old)) (iter (cdr old) (append new (list (car old) (car old))))]
          [(p1 (car old)) (iter (cdr old) (append new (list (car old))))]
          [else
           (iter (cdr old) new)])))
  (iter liste '()))

(arbeiten2 '(1 2 "a" 3 "b") number? string?)
