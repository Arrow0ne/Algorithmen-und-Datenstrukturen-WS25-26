#lang racket/base

(define pi 3.141592653589793)

(define (regenmesser durchmesser hoehe menge)
  (if (or (< durchmesser 0) (< hoehe 0) (< menge 0))
      (0)
      (let* ((flaeche (* pi (expt (/ durchmesser 2) 2)))
             (volumen-zylinder (* flaeche hoehe))
             (hoehe (/ menge flaeche))
             (h-regen (* 0.1 menge))
             (volumen-regen (* flaeche h-regen)))
        (if (> volumen-regen volumen-zylinder)
            0
            h-regen))))

(regenmesser 100 20 1)
(regenmesser 100 20 5)
(regenmesser 200 10 20)

(define (f a b c d e)
  (or (and a b d e) (and c a (not d)) (and c (not a))))
(f #f #f #f #t #t)


(define (volumen a b c d e)
  (if (or (< a 0) (< b 0) (< c 0) (< d 0) (< e 0))
      (-1)
  (let* ((flaeche-quadrat (* (+ (* b 2) d) e))
        (volumen-quadrat (* flaeche-quadrat c))
        (flaeche-dreieck (* 0.5 d a))
        (volumen-dreieck (* flaeche-dreieck c))
        (volumen-quadrei (- volumen-quadrat volumen-dreieck))
        (flaeche-halbkreis (* 0.5 (expt a 2) pi))
        (volumen-halbzylinder (* flaeche-halbkreis c))
        (volumen-gesamt (+ volumen-quadrei volumen-halbzylinder)))
  volumen-gesamt)))

(volumen 2 2 3 4 3)

(define (anzahl a b c)
  (if (= a 0)
  #f
  (let ((D (- (expt b 2) (* 4 a c))))   (cond
      ((> D 0) 2)
      ((= D 0) 1)
      ((< D 0) 0)))))

(anzahl 1 1 -2)
(anzahl 0 3 2)

(define (f1 a b c d e)
  (or (and a b d e) (and (not (and c a d)) (and (or (and a (not e)) (and (not a) e)) c))))

(f1 #f #f #f #t #t)


(define (f2 a b c d)
  (or (and a b d) a (and c d) (not (and a c))))

(f2 #f #f #f #t)

(define (pruefeZahl n)
  (if (and (> n 12) (<= (* n n) 999) (= (remainder n 3) 0))
  #t
  #f))

(pruefeZahl 13)
(pruefeZahl 15)

(define (skonto betrag anzahltage)
  (cond ((<= anzahltage 10) (- betrag (* betrag 0.03)))
        ((<= anzahltage 20) (- betrag (* betrag 0.02)))
        (else betrag)))

(skonto 100 15)


(define (f3 a b c d e)
  (or (and a b d e) (and (not (or c a d)) (not (or a c)))))

(f3 #f #f #f #t #t)

(define (tetraederzahl n)
  (/ (* n (+ n 1) (+ n 2)) 6))

(tetraederzahl 1)
(tetraederzahl 6)

(define (preis kwh)
  (cond ((< kwh 2000) (* kwh (+ 0.2 (* 0.2 0.1))))
        ((> kwh 3500) (* kwh (- 0.2 (* 0.2 0.05))))
        (else (* kwh 0.2))))

(preis 2500)
(preis 1800)
(preis 3600)