(define right-split (split beside below))
(define up-split (split below beside))

(define (split ls rs)
  (lambda (painter)
    (beside (ls painter) (rs painter))
    )
  )