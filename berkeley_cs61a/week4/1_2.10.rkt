(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
	   (>= (upper-bound y) 0))
      (error "Can't divide by an interval that spans zero.")
      (mul-interval x
		    (make-interval (/ 1 (upper-bound y))
				   (/ 1 (lower-bound y))))))