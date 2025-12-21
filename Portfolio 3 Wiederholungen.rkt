#lang racket
(define (verkettung prozedur-liste argument)
  (define (iter ls ergebnis)
    (if (null? ls)
        (apply max ergebnis)
        (iter (cdr ls) (cons ((car ls) argument) ergebnis))))
  (iter prozedur-liste '()))

(define prozeduren (list
                    abs
                    (lambda (x) (* x x))
                    (lambda (x) (+ x 1))
                    ))
(verkettung prozeduren 6)

(define (generator prozedur start ende)
  (define (iter counter ls)
    (if (> counter ende)
        (reverse ls)
        (iter (+ counter 1) (cons (prozedur counter) ls))))
  (iter start '()))
(define (quadrat x)
  (* x x))
(generator quadrat 1 5)

(define (arbeiten liste praedikat1 praedikat2)
  (define (iter old new)
    (if (null? old)
        new
        (cond
         [(and (praedikat1 (car old)) (praedikat2 (car old))) (iter (cdr old) (append new (list (car old) (car old))))]
         [(praedikat2 (car old)) (iter (cdr old) (append new (list (car old) (car old))))]
         [(praedikat1 (car old)) (iter (cdr old) new)]
         [else
          (iter (cdr old) (append new (list (car old))))])))
  (iter liste '()))
(arbeiten '(1 2 "Hallo" #t sin) number? string?)

(define (operation operatoren n)
  (lambda (x)
    (define (iter counter old operator)
      (if (> counter n)
          (apply operator x)
          (iter (+ counter 1) (cdr old) (car old))))
    (iter 1 operatoren '())))

(define plus (operation (list + - * /) 1))
(plus (list 1 2))

(define (rotiere liste n)

  ;; lokale Hilfsfunktion
  (define (ans-ende ls x)
    (if (null? ls)
        (list x)
        (cons (car ls) (ans-ende (cdr ls) x))))

  ;; lokale Iteration
  (define (iter counter ls)
    (if (= counter n)
        ls
        (iter (+ counter 1)
              (ans-ende (cdr ls) (car ls)))))

  (iter 0 liste))

(rotiere '(1 2 3) 1)


(define (norm10 x) ;bleibt zwischen 0 und 9
  (modulo x 10))

;(define (caesar_encrypt_list data key)
 ; (define (iter keys olddata ls)
  ;  (cond
   ;   [(null? olddata) (reverse ls)]
    ;  [(null? keys) (iter key olddata ls)]
     ; [else
      ; (iter (cdr keys) (cdr olddata) (cons (norm10 (+ (car olddata) (car keys))) ls))]))
 ; (iter key data '()))

; (caesar_encrypt_list (list 1 2 3 4 5 6) (list 1 3 3 7))

(define (caesar_encrypt_advanced data key mode)
  (define (iter keys olddata new)
    (cond
      [(null? olddata) (reverse new)]
      [(null? keys) (iter key olddata new)]
      [(eq? mode 'encrypt) (iter (cdr keys) (cdr olddata) (cons (norm10 (+ (car olddata) (car keys))) new))]
      [(eq? mode 'decrypt) (iter (cdr keys) (cdr olddata) (cons (norm10 (- (car olddata) (car keys))) new))]
      [else
       "Falsche Eingabe"]))
  (iter key data '()))

(caesar_encrypt_advanced (list 1 2 3 4 5 6) (list 1 -3 3 7) 'encrypt)
(caesar_encrypt_advanced (list 2 9 6 1 6 3) (list 1 -3 3 7) 'decrypt)

(define (caesar text k)
  (define (iter chars acc)
    (if (null? chars)
        (list->string (reverse acc))
        (iter (cdr chars)
              (cons (shift-char (car chars) k) acc))))
  (iter (string->list text) '()))

(define (shift-char c k)
  (integer->char
   (+ 97
      (modulo (+ (- (char->integer c) 97) k) 26))))

(caesar "abc" 2)
(caesar "xyz" 3)

(define (progressiver-caesar text k)
  (define (iter chars acc counter)
    (if (null? chars)
        (list->string (reverse acc))
        (iter (cdr chars) (cons (shift-char (car chars) counter) acc) (+ counter k))))
  (iter (string->list text) '() k))

(progressiver-caesar "aaaa" 1)
(progressiver-caesar "aaaa" 2)

