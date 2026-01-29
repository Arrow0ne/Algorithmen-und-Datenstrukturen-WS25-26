#lang racket

;Aufgbae 4
(define (groesster-vektor v1 v2)
  (if (> (vector-length v1) (vector-length v2))
      v1
      v2))

(define (vector-add vec1 vec2)
  (define (iter count newvec n)
    (if (< count n)
        (begin
          (cond
            ((>= count (vector-length vec1))
             (vector-set! newvec count (vector-ref vec2 count)))
            ((>= count (vector-length vec2))
             (vector-set! newvec count (vector-ref vec1 count)))
            (else
             (vector-set! newvec count (+ (vector-ref vec1 count) (vector-ref vec2 count)))))
          (iter (+ count 1) newvec n))
        newvec))
  (iter 0 (make-vector (vector-length (groesster-vektor vec1 vec2))) (vector-length (groesster-vektor vec1 vec2))))

(vector-add #(1 2 3) #(4 5 6))
(vector-add #(1 1) #(4 5 6 7))

;Aufgabe 5
(define (swap vec pos1 pos2)
  (let ((tmp (vector-ref vec pos2)))
    (vector-set! vec pos2 (vector-ref vec pos1))
    (vector-set! vec pos1 tmp)))

(define (bubble-sort vec comparator)
  (define (iter idx swapped)
    (cond ((>= idx (- (vector-length vec) 1)) (if (not swapped)
                                                  vec
                                                  (iter 0 #f)))
          ((comparator (vector-ref vec idx) (vector-ref vec (+ idx 1)))
           (begin
             (swap vec idx (+ idx 1))
             (iter (+ idx 1) #t)))
          (else
           (iter (+ idx 1) swapped))))
  (iter 0 #f))
    
(bubble-sort (vector 9 1 8 4 8 1 5 9 23 3 22 0) >)
(bubble-sort (vector 9 1 8 4 8 1 5 9 23 3 22 0) <)

;Aufgabe 6
(define (vector-apply start operator vecVal vecIndices)
  (define (iter counter ergebnis)
    (if (> counter (- (vector-length vecIndices) 1))
        ergebnis
        (iter (+ counter 1) (operator ergebnis (operator start (vector-ref vecVal (vector-ref vecIndices counter)))))))
  (iter 0 start))

(vector-apply 0 + #(1 2 3 4) #(0 1))
(vector-apply 1 * #(3 1 2 9) #(2 1 0))