#lang racket
;Aufgabe 1
(define (operator x)
  (cond
    ((eq? x '+) +)
    ((eq? x '-) -)
    ((eq? x '*) *)
    ((eq? x '/) /)))

(define (pruefe symbol zuweisung)
  (if (eq? symbol (caar zuweisung))
       (cadar zuweisung)
  (pruefe symbol (cdr zuweisung))))

(define (wert x zuweisung)
  (if (number? x)
      x
      (pruefe x zuweisung)))

(define (werte-aus term zuweisung)
  ((operator (car term))
   (wert (cadr term) zuweisung)
   (wert (caddr term) zuweisung)))

(werte-aus '(+ x 2) '((x 7)))
(werte-aus '(* x y) '((x 3) (y 5)))
(werte-aus '(/ a b) '((a 6) (b 3)))

;Aufgabe 2
(define (deep-memq element liste)
  (cond
    ((null? liste) #f)
    ((eq? element (car liste)) #t)
    ((list? (car liste))
     (or (deep-memq element (car liste))
         (deep-memq element (cdr liste))))
    (else
     (deep-memq element (cdr liste)))))

(deep-memq 2 '(1 2 3))
(deep-memq 3 '(1 (2 (4 5) 3)))
(deep-memq 3 '((1 5) (2 (7 2 6 4 (4 5) (2 4)))))

;Aufgabe 3
(define (alle-kleineren grenze liste)
  (cond
    ((null? liste) '())
    ((number? (car liste))
     (if (< (car liste) grenze)
         (cons (car liste)
               (alle-kleineren grenze (cdr liste)))
         (alle-kleineren grenze (cdr liste))))
    ((list? (car liste))
     (append (alle-kleineren grenze (car liste))
             (alle-kleineren grenze (cdr liste))))
    (else
     (alle-kleineren grenze (cdr liste)))))

(alle-kleineren 4 '((7 2 (3 40)) (3 4 (2 3 (9)))))
(alle-kleineren 3 '((3 4 (6 3 (9)) 8)))
(alle-kleineren 22 '((19 20) (21 22) (23 24)))
;Aufgbae 4
;_________
