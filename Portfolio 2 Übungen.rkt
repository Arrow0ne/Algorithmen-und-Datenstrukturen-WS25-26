#lang racket
;Portfolio Übung WS 23/24
;Aufgabe 1

(define (zaehleteiler n)
  (define (iter counter zahl)
    (if (> n 0)
        (if (< zahl n)
            (if (= (remainder n zahl) 0)
                (iter (+ counter 1) (+ zahl 1))
                (iter counter (+ zahl 1)))
            counter)
        #f))
  (iter 0 2))

(zaehleteiler 10)
(zaehleteiler 16)

;Aufgabe 2

(define (linker-index ziffer zahl)
  (define (iter counter n r)
    (if (> n 0)
        (let* ((next-n  (quotient n 10))
               (digit   (remainder n 10))
               (new-r   (if (= digit ziffer)
                            counter
                            r)))
          (iter (+ counter 1) next-n new-r))
        (if (= r 0)
            #f
            (- counter r))))
  (iter 1 zahl 0))

(linker-index 1 531863)
(linker-index 8 1234567890)


;Portfolio Übung WS 24/25
;Aufgabe 1

(define (einstellige-quersumme zahl)
  (define (iter summe n)
    (if (= n 0)
        summe
        (let* ((next-n (quotient n 10))
               (ziffer (remainder n 10))
               (new-summe (+ summe ziffer)))
          (iter new-summe next-n))))
  (iter 0 zahl))

(einstellige-quersumme 123)
(einstellige-quersumme 1234)
(einstellige-quersumme 12345)

;Aufgabe 2

(define (vergleich zahl op)
  (define (iter ziffer zf n counter)
    (if (= n 0)
        counter
        (let* ((next-n (quotient n 10))
               (new-ziffer (remainder n 10))
               (new-zf (remainder next-n 10)))
          (if (and (op new-zf new-ziffer) (> (quotient n 10) 0))
              (iter new-ziffer new-zf next-n (+ counter 1))
              (iter new-ziffer new-zf next-n counter)))))
  (iter 0 0 zahl 0))
    
(vergleich 112233 <)

