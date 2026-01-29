#lang racket

;WS 2024/25
;Aufgabe 1
(define (addiere vektor1 . vektoren)
  (define (iter old new counter)
    (if (null? old)
        new
    (cond
      ((< (length vektoren) 1) #f)
      ((= (length (cons vektor1 vektoren)) 1) vektor1)
      ((null? vektoren) new)
      ((= counter 3) (iter (cdr old) new (- counter 3)))
      ((<= counter 2) (begin
                        (vector-set! new counter
                                     (+ (vector-ref new counter)
                                        (vector-ref (car old) counter)))
                                        (iter old new (+ counter 1)))))))
  (iter vektoren (vector-copy vektor1) 0))

;Aufgabe 2
(define (tauschen vektor)
  (define n (vector-length vektor))
  (define half (quotient n 2))
  (define result-length (* 2 half))
  (define result (make-vector result-length))

  (define (iter i)
    (cond
      ((= i half) result)
      (else
       (begin
         (vector-set! result i
           (vector-ref vektor (+ i half (if (odd? n) 1 0))))
         (vector-set! result (+ i half)
           (vector-ref vektor i))
         (iter (+ i 1))))))

  (iter 0))


(tauschen (vector 1 2 3 4))
(tauschen (vector 1 2 3 4 5))
(tauschen (vector 1 2 3 4 5 6 7))

;WS 2023/24


  