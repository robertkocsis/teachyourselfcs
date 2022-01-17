(define (make-interval a b) (cons a b))

(define (lower-bound pair) (car pair))

(define (upper-bound pair) (cdr pair))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent x)
  (let (
        (tolerance (- (upper-bound x) (center x))))
        (* 100 (/ tolerance (upper-bound x)))))

(percent (make-interval 3.35 3.65))

(define (make-center-percent cen per)
  (make-interval (- cen (* per (/ cen 100)))
                 (+ cen (* per (/ cen 100))))
  )

(make-center-percent 3.5 (percent (make-interval 3.35 3.65)))