#lang simply-scheme
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define my-tree (make-tree 8 (make-tree 5 '() '())
                           (make-tree 12 '() '())))

(define (max-fanout tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else
         (max (- (length tree) 1) (max-fanout (car tree)) (max-fanout (cdr tree)) )
         )
        )
  )

;(max-fanout my-tree) ;2

(define fanout-tree
  '( 8 (5 1 2 3 4 6) (12 '() '())))

(max-fanout fanout-tree)







