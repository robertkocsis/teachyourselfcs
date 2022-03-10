(load "obj.rkt")

(define-class (random-generator range)
  (instance-vars (count 0))
  (method (number)
          (set! count (+ 1 count))
          (random range))
  )

(define r10 (instantiate random-generator 10))

(ask r10 'count)

(ask r10 'number)

(ask r10 'number)

(ask r10 'count)