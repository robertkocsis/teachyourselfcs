(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess original)
    (if (good-enough? guess original)
         guess
         ((iterative-improve good-enough? improve-guess) (improve-guess guess original) original))
     ))

(define (sqrt x)
  ((iterative-improve good-enough? improve) 1.0 x))

(sqrt 2)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? improve) 1.0 x)))


