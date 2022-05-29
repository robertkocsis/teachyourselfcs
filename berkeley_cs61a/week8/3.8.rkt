#lang simply-scheme

(define f (let ((invocations 0))
            (lambda (x)
              (if (= invocations 0)
                  (begin
                    (set! invocations 1)
                    x
                    )
                  0
                  )
              )
            )
  )

(+ (f 0) (f 0)); from left to right -> 0 from right to left -> 1