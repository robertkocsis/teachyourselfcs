#lang sicp

(define (count-pairs og)
  (let ((pairs '())
        (count 0))
    (define (count-pair pair)
      (set! pairs (cons pair pairs))
      (set! count (+ count 1))
      )
    (define (traverse-pair pair)
      (cond ((not (pair? pair)) #f)
            ((memq pair pairs) #f)
            (else (count-pair pair)
                  (traverse-pair (car pair))
                  (traverse-pair (cdr pair)))
          
            )
      )
    (traverse-pair og)
    count))

(define a '(1 2 3))

(count-pairs a)

(define b '(1 2 3))
(set-car! (cdr b) (cddr b))

(count-pairs b)

(define c '(1 2 3))
(set-car! (cdr c) (cddr c))
(set-car! c (cdr c))

(count-pairs c)


(define d '(1 2 3))
(set-car! (cdr d) (cdr d))
(count-pairs d)