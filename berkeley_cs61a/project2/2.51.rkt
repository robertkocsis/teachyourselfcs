(load "2.50.rkt")
(load "2.49.rkt")
(load "picture.rkt")


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 1.5 0.5))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))