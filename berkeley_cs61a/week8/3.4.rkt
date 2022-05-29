#lang simply-scheme
(define (make-account balance password)
  (let ((pass-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass m)
      (cond ((not (eq? pass password))
             (set! pass-count (+ 1 pass-count))
             (if (< 7 pass-count) "Cops Called" "Incorrect password")
             )
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define acc (make-account 100 'secret-password))