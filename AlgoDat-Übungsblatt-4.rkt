#lang racket/base

; odd? Prädikat, das eine Zahl darauf testet, ob sie ungerade ist
; even ? Prädikat, das eine Zahl darauf testet, ob sie gerade ist
; integer ? Prädikat, das eine Zahl darauf testet, ob sie ganzzahlig ist
; sqrt Berechnet die Quadratwurzel einer Zahl
; ceiling Die Ceiling-Funktion (Aufrundungsfunktion) liefert zu einer reellen Zahl die nächstgrößere
; ganze Zahl. Die mathematische Schreibweise dafür ist ⌈ ⌉

;------------------------------------------

(define (ganzzahlige-wurzel? n)
  (integer? (sqrt n))) ;prüft ob die quadratwurzel von n eine ganzzahl ist

(ganzzahlige-wurzel? 25)
(ganzzahlige-wurzel? 24)
(ganzzahlige-wurzel? 16)

;------------------------------------------

(define (primzahl? n)
  (define (iter n counter)
    (if (<= counter (sqrt n))
        (if (= (remainder n counter) 0)
        #f
        (iter n (+ counter 1)))
    #t))
  (iter n 2))

(primzahl? 11)
(primzahl? 26737)
(primzahl? 200)
(primzahl? 121)

;------------------------------------------

(define (kubiksumme n)
  (define (iter zahl summe)
    (if (= zahl 0)
        (expt summe 3)
    (iter (quotient zahl 10) (+ summe (remainder zahl 10)))))
  (iter n 0))

(kubiksumme 101042)
(kubiksumme 34567)

;------------------------------------------
(define (reverse-number n) ;reverse funktion n ist in dem fall von unten das n
  (define (iter zahl ergebnis)
    (if (= zahl 0)
        ergebnis
        (iter (quotient zahl 10)
              (+ (* ergebnis 10) (remainder zahl 10))))) ; der reverse
  (iter n 0)) ; festlegung

(define (caesar_encrypt n k)
  (define (iter zahl summe)
    (if (= zahl 0)
        summe
        (iter (quotient zahl 10) (+ (* summe 10) (modulo (+ (remainder zahl 10) k) 10))))) ;das encrypten modulo ist da damit man nicht auf 10 kommt sondern immer zwischen 0-9 bleibt
  (reverse-number(iter n 0))) ;hier wird reverse number aufgerufen und für n das eingesetzt was da steht

(caesar_encrypt 1234 1)
(caesar_encrypt 7901 2)
(caesar_encrypt 987 1)
