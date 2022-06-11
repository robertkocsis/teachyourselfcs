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

(define (vector-filter f vect-to-filter)
  (let ((result (make-vector 0)))
    (define (iter-vec vec i)
      (cond ((equal? i (vector-length vec)) #t)
            ((f (vector-ref vec i)) (set! result (vector-append result (vector (vector-ref vec i)))))
            (else (iter-vec vec (+ i 1))))
      )
    
    (iter-vec vect-to-filter 0)
    result
    )
  )

(define a (vector 1 2 3))

(vector-filter even? a)