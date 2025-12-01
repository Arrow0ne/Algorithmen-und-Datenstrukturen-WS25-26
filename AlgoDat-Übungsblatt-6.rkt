#lang racket

;Aufgabe 1

(define (zaehlen start ende n)
  (define (helper current count)
    (cond
      [(> current ende) 0]
      [(and (= 0 (modulo current 3))
            (= 0 (modulo current 7)))
       (if (= count n)
           current
           (helper (+ current 1) (+ count 1)))]
      [else (helper (+ current 1) count)]))
  (helper start 1))


(zaehlen 10 100 1)
(zaehlen 10 100 2)
(zaehlen 10 100 3)
(zaehlen 10 100 1337)

;Aufgabe 2

(define (gleiche-ziffern zahl)
  (define (erste-ziffer x)
    (if (< x 10)
        x
        (erste-ziffer (quotient x 10))))

  (define (letzte-ziffer x)
    (modulo x 10))

  (if (= (erste-ziffer zahl) (letzte-ziffer zahl))
      zahl
      (gleiche-ziffern (+ zahl 1))))

(gleiche-ziffern 123)
(gleiche-ziffern 4567)

;Aufgabe 3

(define (konst-addierer n)
  (lambda (a)
    (+ a n)))

(define plus1 (konst-addierer 1))
(plus1 98)
(define plus10 (konst-addierer 10))
(plus10 98)

;Aufgabe 4

(define (konst-ggt b)
  (lambda (a)
    (define (ggt x y)
      (if (= y 0)
          x
          (ggt y (modulo x y))))
    (ggt a b)))

(define ggt10 (konst-ggt 10))
(ggt10 25)
(ggt10 27) 

(define ggt987 (konst-ggt 987))
(ggt987 762351)
(ggt987 98123746)

;Aufgabe 5

(define (paar-operation op)
  (cond
    [(eq? op =) (lambda (p)
                   (define a (car p))
                   (define b (cdr p))
                   (= a b))]
    [(eq? op +) (lambda (p)
                   (define a (car p))
                   (define b (cdr p))
                   (+ a b))]
    [(eq? op -) (lambda (p)
                   (define a (car p))
                   (define b (cdr p))
                   (- a b))]
    [(eq? op <) (lambda (p)
                   (define a (car p))
                   (define b (cdr p))
                   (< a b))]
    [(eq? op >) (lambda (p)
                   (define a (car p))
                   (define b (cdr p))
                   (> a b))]))

(define paar=? (paar-operation =))
(paar=? (cons 2 3))
(paar=? (cons 3 3)) 

(define paar<? ( paar-operation <))
(paar<? (cons 2 3))
(paar<? (cons 3 3))

(define paar+ ( paar-operation +))
(paar+ (cons 2 3))
(paar+ (cons 3 3))

;Aufgabe 6

