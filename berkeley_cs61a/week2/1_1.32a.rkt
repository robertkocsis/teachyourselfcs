(define (product fn a next b)
  (if (> a b)
      1
      (* (fn a)
         (product fn (next a) next b))
      )
  )



(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b)))
  )

(define (newProduct fn a next b)
  (accumulate (lambda (x y) (* x y)) 1 fn a next b))

(define (pi-approx approx)
  (* 4 (newProduct (lambda (x)
                     (if (even? x)
                         (/ x (+ x 1.0))
                         (/ (+ x 1.0) x))
                     )
                   2
                   (lambda (x) (+ x 1))
                   approx)))

(pi-approx 5000)


