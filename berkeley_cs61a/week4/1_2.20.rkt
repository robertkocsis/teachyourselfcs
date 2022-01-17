(define (same-parity start . rest)
  (let ((func
         (if (even? start)
             (lambda (x) (even? x))
             (lambda (x) (odd? x))))

       )
    (define (filter x)
      (cond ((empty? x) null)
            ((func (first x)) (cons (first x) (filter (bf x))))
            (else (filter (bf x)))
            ))
    (cons start (filter rest))
     
    )
  )

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

