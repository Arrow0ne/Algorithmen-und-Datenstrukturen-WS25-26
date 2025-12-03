#lang racket

;Aufgabe 1

(define (loesche liste praedikat)
  (if (praedikat (car liste))
      (loesche (cdr liste) praedikat)
      liste))


(loesche (list 4 6 8 3 2 4 5) even?)
(loesche (list 7 5 3 2 4 5 9) odd?)

;Aufgabe 2

(define (drehe liste)
  (define (iter alt neu)
    (if (null? alt)
        neu
        (iter (cdr alt) (cons (car alt) neu))))
  (iter liste (list)))

(drehe (list 1 2 3))
(drehe (list 1 2 (list 3)))
(drehe (list 1 (list 5 6) 2 (list 3 4)))

;Aufgabe 3

(define (typ-or typ1 typ2)
  (lambda (a)
    (or (typ1 a) (typ2 a)))) ;or wichtig sonst falsche Ergebnisse

(define paar-oder-liste? (typ-or pair? list?))
(paar-oder-liste? (cons 1 2))

(define integer-oder-boolean? (typ-or integer? boolean?))
(integer-oder-boolean? (paar-oder-liste? (cons 1 2)))

;Augabe 4

(define (operation operatoren n)
  (lambda (a)
    (define (iter counter old new)
          (if (> counter n)
              new
              (iter (+ 1 counter) (cdr old) (cons (car old) new))))
    (define hilfe (iter 1 operatoren '()))
    (apply (car hilfe) a)))

(define plus (operation (list + - * /) 1))
(plus (list 1 2))

;Aufgabe 5

(define (caesar_encrypt_list data key)
  

(caesar_encrypt_list (list 1 2 3 4 5 6) (list 1 3 3 7))