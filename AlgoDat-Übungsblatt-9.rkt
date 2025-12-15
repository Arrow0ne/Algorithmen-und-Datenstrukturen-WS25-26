#lang racket

(define (tuerme-von-hanoi n)
  (define (tuerme-iter n start via ende)
    (cond ((< n 1) '())
           (else (append (tuerme-iter (- n 1) start ende via) (list (cons start ende)) (tuerme-iter (- n 1) via start ende)))))
  (tuerme-iter n 'a 'b 'c))

(tuerme-von-hanoi 3)

;(define (liste-teilen eingabe)
;  (define (teilen e liste1 liste2 zaehler)
;    (cond
;      ((null? e) (list liste1 liste2))
;      ((even? zaehler) (teilen (cdr e) (append liste1 (list (car e))) liste2 (+ zaehler 1)))
;      (else (teilen (cdr e) liste1 (append liste2 (list car e)) (+ zaehler 1))))
;    )
;  (teilen eingabe '() '() 0))
;
;(liste-teilen '(1 2 3 4 5 6 7 8 9))
;(liste-teilen '(1 2 3 4 5 6 7 8 9 10 11 12 13))

