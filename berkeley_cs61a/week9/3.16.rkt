#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define a '(1 2 3))

(count-pairs a)

(define b '(1 2 3))
(set-car! (cdr b) (cddr b))

(count-pairs b)

(define c '(1 2 3))
(set-car! (cdr c) (cddr c))
(set-car! c (cdr c))

(count-pairs c)


(define d '(1 2 3))
(set-car! (cdr d) (cdr d))
;(count-pairs d)