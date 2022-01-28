(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b) (eq? a b)) #t)
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((and (null? a) (null? b) (eq? a b)) #t)
        (else #f)
        )
  )

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))



