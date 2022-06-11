#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))


(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 


(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)  ; (cons (cons 'a '()) (cons 'a '())) -> ((a) a) lisp represents the cdr without the ()
(insert-queue! q1 'b)  ; adding b will add it to the cdr of car queue and to cdr of queue too
;(insert-queue! q1 'c)
;(delete-queue! q1)
;(delete-queue! q1)   ; basically the front part of the queue contains all the queue items and the cdr of the queue contains the last item

(front-ptr q1)
(rear-ptr q1) 
q1

(define (print-queue queue)
  (display (car queue)) (newline)
  )

(print-queue q1)