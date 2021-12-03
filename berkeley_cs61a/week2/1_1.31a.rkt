(define (product fn a next b)
  (if (> a b)
      1
      (* (fn a)
         (product fn (next a) next b))
      )
  )


(define (factorial a)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1))  a)
  )

(factorial 5)

(define (pi-approx approx)
  (* 4 (product (lambda (x)
                  (if (even? x)
                      (/ x (+ x 1.0))
                      (/ (+ x 1.0) x))
                  )
                2
                (lambda (x) (+ x 1))
                approx)))

(pi-approx 5000)