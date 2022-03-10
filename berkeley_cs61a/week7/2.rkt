(load "obj.rkt")

(define-class (coke-machine max-cans price)
  (instance-vars (deposited-money 0) (can-count 0))
  (method (deposit amount)
          (set! deposited-money (+ amount deposited-money)))
  (method (coke)
          (cond ((= can-count 0) '(Machine empty))
                ((< deposited-money price) '(Not enough money))
                (else 
                       (set! can-count (- can-count 1))
                       (set! deposited-money (- deposited-money price))
                       deposited-money
                       )
                )
          )
  (method (fill amount)
          (set! can-count (+ amount can-count))))

(define my-machine (instantiate coke-machine 80 70))

(ask my-machine 'fill 60)
(ask my-machine 'deposit 25)
(ask my-machine 'coke)
(ask my-machine 'deposit 25) ;; Now there’s 50 cents in there.
(ask my-machine 'deposit 25) ;; Now there’s 75 cents.
(ask my-machine 'coke)


