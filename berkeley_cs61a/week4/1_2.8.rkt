(define (make-interval a b) (cons a b))

(define (lower-bound pair) (car pair))

(define (upper-bound pair) (cdr pair))

(define (sub-interval a b)
  
   (make-interval
    (- (lower-bound a) (upper-bound b))
    (- (upper-bound a) (lower-bound b))
    )
   )

(sub-interval (make-interval 4 9) (make-interval 1 4))