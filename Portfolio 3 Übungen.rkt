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
  (lambda (a)
    (define (iter counter operator ls)
      (if (< counter n)
          (iter (+ counter 1) (car ls) (cdr ls))
          (apply  operator a)))
    (iter 0 '() operatoren)))

(define plus (operation (list + - * /) 1))
(plus (list 1 2))
(define mal (operation (list + - * /) 3))
(mal (list 10 10))
(define minus (operation (list + - * /) 2))
(minus (list 10 5))
(define geteilt (operation (list + - * /) 4))
(geteilt (list 4 2))

;Aufgabe 2 
; bei einer (1) verschiebung 1 2 3 ->(nach 1.) 2 3 1 ->(nach 2.) 3 1 2 ->(nach 3.) 1 2 3
; car teil von alter wird cdr von neuer und cdr von alter wird car von neuer liste
(define (ans-ende ls x) ;Hilfsfunktion
  (if (null? ls)
      (list x)
      (cons (car ls) (ans-ende (cdr ls) x))))

(define (rotiere liste n)
  (define (iter counter ls)
    (if (= counter n)
        ls
        (iter (+ counter 1) (ans-ende (cdr ls) (car ls)))))
  (iter 0 liste))

(rotiere '(1 2 3) 1)
(rotiere '(1 2 3) 2)
(rotiere '(1 2 3) 3)
(rotiere '(1 2 3 4 5) 1)

;Aufgabe 3
;(define (struktur n)
; (cond
    
    
;(struktur 0)

;Wintersemester 2023/2024
;Aufgabe 1
(define (anpassen liste)
  (define (iter lis ls)
    (if (null? lis)
        ls
        (cond
          [(odd? (car lis)) (iter (cdr lis) ls)]
          [(eq? (modulo (car lis) 10) 0) (iter (cdr lis) (append ls (list (* (car lis) (car lis)))))]
          [else
           (iter (cdr lis) (append ls (list (car lis))))])
        )
    )
  (iter liste '()))

(anpassen (list 5 9 10 12 20))

;Aufgabe 2
(define (gleich? liste)
  (define (iter neg pos lis)
    (if (null? lis)
        (if (eq? (length neg) (length pos))
            #t
            #f)
        (cond
          [(< (car lis) 0) (iter (append neg (list (car lis))) pos (cdr lis))]
          [(> (car lis) 0) (iter neg (append pos (list (car lis))) (cdr lis))])
        )
    )
  (iter '() '() liste))

(gleich? '(1 3 4 -2 3 -5 -6 -7)) ; -> pos 4 neg 4 -> #t
(gleich? '(1 2 3 4 -5 -6 -7 -8 -9)) ; -> pos 4 neg 5 -> #f

;Aufgabe 3
(define (sortieren liste praedikat)
  (define (iter lis newlis hilfelis)
    (if (null? lis)
        (append newlis hilfelis)
        (if (praedikat (car lis))
            (iter (cdr lis) (append newlis (list (car lis))) hilfelis)
            (iter (cdr lis) newlis (append hilfelis (list (car lis)))))))
  (iter liste '() '()))

(sortieren '(1 2 3 4 5 6 7 8 9) odd?)
(sortieren '(1 2 3 4 5 6 7 8 9) even?)

;Wintersemester 2024/2025
;Aufgabe 1
(define (kombiniere op)
  (lambda (liste1 liste2)
    (define (iter lis1 lis2 ergebnis)
      (if (and (null? lis1) (null? lis2))
          ergebnis
          (cond
            [(null? lis1) (iter lis1 (cdr lis2) (append ergebnis (list (op 0 car lis2))))]
            [(null? lis2) (iter (cdr lis1) lis2 (append ergebnis (list (op (car lis1) 0))))]
            [else
             (iter (cdr lis1) (cdr lis2) (append ergebnis (list (op (car lis1) (car lis2)))))])))
    (iter liste1 liste2 '())))

(define (op a b) (+ a b))
(define func (kombiniere op))
(func '(1 2 3 1 2 3) '(3 4 5))

;Aufgabe 2
(define (summen-verkettung prozedur-liste argument)
  (define (iter prozedur ergebnis)
    (if (null? prozedur)
        (apply + ergebnis)
        (iter (cdr prozedur) (append ergebnis (list ((car prozedur) argument))))
        )
    )
  (iter prozedur-liste '()))

(define prozeduren2 (list abs (lambda (x) (* x x)) (lambda (x) (+ x 1))))
(summen-verkettung prozeduren2 5)

;Aufgabe 3
(define (dist p q)
  (sqrt (+ (expt (- (car p) (car q)) 2)
           (expt (- (cdr p) (cdr q)) 2))))

(define (naechster-nachbar liste punkt)
  (define (iter ls beste-dist nachbarn)
    (cond
      [(null? ls)
       (if (null? nachbarn) #f nachbarn)]
      [else
       (let ([d (dist (car ls) punkt)])
         (cond
           [(< d beste-dist)
            (iter (cdr ls) d (list (car ls)))]
           [(= d beste-dist)
            (iter (cdr ls) beste-dist (cons (car ls) nachbarn))]
           [else
            (iter (cdr ls) beste-dist nachbarn)]))]))

  (if (null? liste)
      #f
      (iter liste +inf.0 '())))

(naechster-nachbar '((0 . 0) (4 . 4)) '(1 . 2))
;; => ((0 . 0))

(naechster-nachbar '((0 . 0) (2 . 4)) '(1 . 2))
;; => ((2 . 4) (0 . 0))   ; gleiche Distanz

(naechster-nachbar '() '(1 . 2))
;; => #f
