(define (last-pair x)
  (if (null? (bf x))
      x
      (last-pair (bf x)))
  )

(last-pair (list 23 72 149 34))
