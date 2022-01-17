(define (make-interval a b) (cons a b))

(define (lower-bound pair) (car pair))

(define (upper-bound pair) (cdr pair))