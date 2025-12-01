#lang racket

(define (nat-wurzel x)
  (do ((sum 0 (+ sum odd))
       (odd 1 (+ odd 2))
       (count 0 (+ count 1)))
((= sum x) count)))

(nat-wurzel 1)
(nat-wurzel 4)
(nat-wurzel 9)
(nat-wurzel 16)
(nat-wurzel 25)
(nat-wurzel 36)

(define (zahl-umdrehen x)
  (define (umdrehen-platz x rev)
    (if (= x 0)
        rev
        (let ((letzte-zahl (remainder x 10)))
          (umdrehen-platz (quotient x 10)
                          (+ (* rev 10) letzte-zahl)))))
    (umdrehen-platz x 0))

(zahl-umdrehen 123)
(zahl-umdrehen 4497821)
(zahl-umdrehen 597050)

(define (aufsteigendes-produkt? a b c d)
  (and (< a b c d) (= (* a b c) d)))

(aufsteigendes-produkt? 1 2 3 6)
(aufsteigendes-produkt? 2 1 3 6)
(aufsteigendes-produkt? 2 3 5 11)

(define (f1 a b)
  (and (not (or a b)) (or a b) (or a (not b))))

(define (f2 a b c)
  (or a (and a b (not c)) ((not a) c) (and (not (and a b)) c)))

(define (f3 a b c d)
  (and (and (or a b) (not (and a b))) (not (or a (not b) c)) (not (and a b c d))))
