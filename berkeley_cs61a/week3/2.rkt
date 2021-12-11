(define (factor? x y)
  (if (= (remainder x y) 0)
      #t #f))

(define (even? n)
  (= (remainder n 2) 0))

(define (sum-of-factors x)
  (define (rec y)
    (cond ((= y 0) 0)
          ((factor? x y) (+ y (rec (- y 1))))
          (else (rec (- y 1)))
          )
    )
  (if (even? x) (rec (/ x 2))
      (rec (/ (+ 1 x) 2)))
      )

(sum-of-factors 6)
(sum-of-factors 28)

(define (next-perf x)
  (define (perf-num? x)
    (if (= (sum-of-factors x) x) #t #f))

  (define (iter number found-perf)
    (cond ((perf-num? number)
           (if (= (+ 1 found-perf) x)
               number
               (iter (+ 1 number) (+ 1 found-perf))
               )
           )
          (else (iter (+ 1 number) found-perf))))

  (iter 1 0)
  )

(next-perf 1)
(next-perf 2)
(next-perf 3)
(next-perf 4)
(next-perf 5)
(next-perf 29)
