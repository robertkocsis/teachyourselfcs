;; I found these online
(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define (relative-prime? x y)
           (= 1 (gcd x y)))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b)))
  )


(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else (combiner null-value
                        (filtered-accumulate combiner null-value term (next a) next b filter)))
        ))

(define (product fn a next b)
  (filtered-accumulate (lambda (x y) (* x y)) 1 fn a next b (lambda (x) (> x 0))))

(define (square-prime-sum a b)
  (filtered-accumulate (lambda (x y) (+ x y))
                       0
                       (lambda (x) (* x x))
                       a
                       (lambda (x) (+ x 1))
                       b
                       (lambda (x) (prime? x))))

(square-prime-sum 0 20)

(define (filtered-product fn a next b filter)
  (filtered-accumulate (lambda (x y) (* x y)) 1 fn a next b filter))

(define (relative-primes-below number)
  (filtered-product
   (lambda (x) (+ x 0))
   2
   (lambda (x) (+ x 1))
   number
   (lambda (x) (and (< x number) (relative-prime? number x))))
  )

(relative-primes-below 15)