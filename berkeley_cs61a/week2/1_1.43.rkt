(define (repeated fn n)
  (define (aggregator iteration)
    (if (= iteration n)
        (lambda (x)  x)
        (lambda (x) (fn ((aggregator (+ iteration 1)) x))
        ))
    )
  (aggregator 0)
  )

(define (square x) (* x x))

((repeated square 2) 5)
