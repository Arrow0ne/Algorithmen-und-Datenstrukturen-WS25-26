#lang racket
;Portfolio 3 Sommersemester 2024

;Aufgabe 1
(define (verkettung prozedur-liste argument)
  (define (iter lst max-wert)
    (cond
      [(null? lst) max-wert] ; Aufhören
      [else
       (let ([ergebnis ((car lst) argument)])
         (iter (cdr lst)
               (max max-wert ergebnis)))]))
  (iter prozedur-liste 0))


  (define prozeduren (list
                      abs
                      (lambda (x) (* x x))
                      (lambda (x) (+ x 1))
                      ))

(verkettung prozeduren 6)

;Aufgabe 2
(define (quadrat x) (* x x))

(define (generator prozedur start ende)
  (define (iter liste counter)
    (if (> counter ende)
        liste
        (iter (append liste (list (prozedur counter))) (+ counter 1))))
  (iter '() start))

(generator quadrat 1 5) 

;Aufgabe 3
(define (arbeiten liste praedikat1 praedikat2)
  (define (iter rest ls)
    (if (null? rest)
        ls
        (let ([element (car rest)])
          (cond
            [(and (praedikat1 element) (praedikat2 element))
             (iter (cdr rest) (append ls (list element element)))]
            [(praedikat1 element) (iter (cdr rest) (append ls (list)))]
            [(praedikat2 element) (iter (cdr rest) (append ls (list element element)))]
            [else
             (iter (cdr rest) (append ls (list element)))]))))
  (iter liste '()))

(arbeiten '(1 2 "Hallo" #t sin) number? string?)

;Sommersemester 2024
;Aufgabe 1
(define (operation operatoren n)
  (