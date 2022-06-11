#lang sicp
(define (vector-append vec1 vec2)
  (let ((result (make-vector (+ (vector-length vec1) (vector-length vec2)))))
    (define (iter-vect vect index res-index)
      (cond ((equal? index (vector-length vect)) #t)
            (else (begin
                    (vector-set! result res-index (vector-ref vect index))
                    (iter-vect vect (+ index 1) (+ res-index 1)))))
      )
    (iter-vect vec1 0 0)
    (iter-vect vec2 0 (vector-length vec1))
    result
    )
  )

(define a (vector 1 2 3))

(define b (vector 4 5 6))

(vector-append a b)