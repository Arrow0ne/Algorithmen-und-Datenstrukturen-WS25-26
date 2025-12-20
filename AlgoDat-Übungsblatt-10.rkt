#lang racket
;Aufgabe 1
(define (removeFirstLast string)
  (substring string 1 (- (string-length string) 1)))

(removeFirstLast "Hallo Welt")
(removeFirstLast "Algorithmik")

;Aufgabe 2
(define (sicheresPasswort passwort)
  (and
   (>= (length (filter (lambda (ch) (not (char-alphabetic? ch))) (string->list passwort))) 2)
   (>= (string-length passwort) 8)
   (ormap char-upper-case? (string->list passwort))
   (ormap char-lower-case? (string->list passwort))
   )
  )

(sicheresPasswort "aUljsb!f/KasDhf")
(sicheresPasswort "ABC123")

;Aufgabe 3
(define (normaler str)
  (sort (filter char-alphabetic? (string->list (string-downcase str))) char<?))

(define (isAnagramm anagramm string2)
  (equal? (normaler anagramm) (normaler string2)))

(isAnagramm "Desperation" "A rope ends it")
(isAnagramm "Eleven plus two" "Twelve plus one")

;Aufgabe 4
(define (vektor-add . vektoren)
  (apply map + vektoren))

(vektor-add '(1 2) '(1 2))
(vektor-add '(1 2 3) '(4 5 6) '(7 8 9))
