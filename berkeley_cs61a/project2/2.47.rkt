(define (make-vect x y) (cons x y))
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2) (cons (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2) (cons (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s) (cons (* (xcor-vect v) s) (* (ycor-vect v) s)))

(define v1 (make-vect 4 4))
(define v2 (make-vect 2 2))

(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect v1 10)