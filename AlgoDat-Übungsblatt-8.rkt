#lang racket

;Aufgabe 1
(define (compress liste)
  (define (iter counter oldlist newlist)
    (cond
      ;; Ende der Liste: das letzte Element finalisieren
      [(null? (cdr oldlist))
       (let ([elem (car oldlist)])
         (reverse
          (if (= counter 1)
              (cons elem newlist)                 ; einzelnes Element -> nur elem
              (cons elem (cons counter newlist)))))] ; Wiederholungen -> elem then count

      ;; nächstes Element gleich -> weiter zählen
      [(eq? (car oldlist) (car (cdr oldlist)))
       (iter (+ counter 1) (cdr oldlist) newlist)]

      ;; neues Element beginnt -> altes in Akkumulator einfügen und count zurücksetzen
      [else
       (let ([elem (car oldlist)])
         (iter 1 (cdr oldlist) (if (= counter 1)
                                   (cons elem newlist)                 ; einzelnes Element
                                   (cons elem (cons counter newlist)))))])) ; elem then count

(if (null? liste)
    '()
    (iter 1 liste '())))

(compress '(a b c))
(compress '(a b b c c c))
(compress '(a b b c c c a b c))
(compress '(a a a a a a a a a a))

;Aufgabe 2
;Aufgabe 3
(define (loeschen liste  n)
  (define (iter counter newlist)
    (if (null? newlist)
        newlist
        (if (= counter n)
            newlist
            (iter (+ counter 1) (cdr newlist)))))
  (iter 0 liste))

(loeschen '(2 3 4 5 6 7) 3)
(loeschen '(2 3 4 5 6 7 8 9 10) 5)
(loeschen '(2 3 4 5 6 7) 8)