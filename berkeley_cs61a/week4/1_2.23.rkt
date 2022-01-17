(define (for-each func items)
  (cond ((empty? items) #t)
        (else (func (car items)) (for-each func (cdr items))))
  )

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))