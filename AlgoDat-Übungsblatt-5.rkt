#lang racket

;Aufgabe 1
(define (fak i)
  (define (iter counter ergebnis)
    (if (< counter i)
        (iter (+ counter 1) (+ ergebnis (* ergebnis counter)))
        ergebnis))
  (iter 1 1))

(define (euler-n n)
  (define (iter count summe)
    (if (>= n 0)
        (if (> count n)
            summe
            (iter (+ count 1.0) (+ summe (/ 1.0 (fak count)))))
        (print "Keine natürliche Zahl")))
  (iter 0 0))

(euler-n 0)
(euler-n 1)
(euler-n 2)
(euler-n 27)


;Aufgabe 2
(define (ackermann n m)
  
  ;n = 0, m > 0
  (define (hilfe1 a b)
    (+ b 1))
  
  ;n > 0, m = 0
  (define (hilfe2 a b)
    (ackermann (- a 1) 1))
  
  ;n > 0, m > 0
  (define (hilfe3 a b)
    (ackermann a (- b 1)))
               
  ;gehört zu hilfe3
  (define (hilfe4 a b)
    (ackermann (- a 1) (hilfe3 a b)))

  (cond
    ((= n 0) (hilfe1 n m))
    ((= m 0) (hilfe2 n m))     
    (else (hilfe4 n m))))

(ackermann 0 0)
(ackermann 0 1)
(ackermann 4 0)
(ackermann 3 1)
(ackermann 3 9)

;Aufgabe 3
(define (osterformel j)
  (let ((a (modulo j 19))
        (b (modulo j 4))
        (c (modulo j 7))
        (k (floor (/ j 100))))
    (let ((p (floor (/ (+ (* 8 k) 13) 25)))
          (q (floor (/ k 4))))
      (let ((M (modulo (- (+ 15 k) p q) 30))
            (N (modulo (- (+ 4 k) q) 7)))
        (let ((d (modulo (+ (* 19 a) M) 30)))
          (let ((e (modulo (+ (* 2 b) (* 4 c) (* 6 d) N) 7)))
            (+ 22 d e)))))))



(osterformel 2010)
(osterformel 2011)
(osterformel 2013)

;Aufgabe 4
(define (maxziffer n)
  (let ((n (abs n))) ;; falls n negativ ist
    (define (iter zahl aktuellesmax)
      (if (= zahl 0)
          aktuellesmax
          (let ((z (modulo zahl 10)))
            (iter (quotient zahl 10)
                  (max z aktuellesmax)))))
    (iter n 0)))

(maxziffer 3475376)
(maxziffer 1012)

;Aufgabe 5
(define (n x) (+ x 1))

(define (sum x y)
  (if (= y 0)
      x
      (n (+ x (- y 1)))))

(sum 3 5)
(sum 20 5)
(sum 25 6)

;Aufgabe 6
(define (mul x y)
  (if (= y 0)
      0
      (sum x (* x (- y 1)))))

(mul 2 2)
(mul 8 8)

;Aufgabe 7
(define (q n)
  (if (< n 3)
      1
      (+ (q (- n (q (- n 1)))) (q (- n (q (- n 2)))))))

(q 15)
(q 16) 
(q 35)