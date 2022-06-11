#lang sicp
(define (bubble-sort! vector)
  (define (compare vec index max-index)
    (let ((a 0)
          (b 0))
      (cond
        ((equal? max-index 0) vec)
        ((<= (+ 1 index) max-index)
         (begin
           (set! a (vector-ref vec index))
           (set! b (vector-ref vec (+ index 1)))
           (if (> a b) (begin
                         (vector-set! vec index b)
                         (vector-set! vec (+ 1 index) a)) #t)
           (compare vec (+ index 1) max-index)
           )
         )
        (else (compare vec 0 (- max-index 1))))
      )
    )
  (compare vector 0 (- (vector-length vector) 1))
  )

(define a (vector 6 5 1 3 2 1))

(bubble-sort! a)