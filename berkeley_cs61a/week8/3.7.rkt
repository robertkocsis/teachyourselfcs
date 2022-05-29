#lang simply-scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (cond ((not (eq? pass password)) "Incorrect password")
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint account og-password new-password)
  (define (dispatch pass m)
    (cond ((not (eq? pass new-password)) "Incorrect password")
          (else (account og-password m))))
  dispatch
  )

(define og-acc (make-account 100 'secret-password))

(define joint-acc (make-joint og-acc 'secret-password 'new-password))

