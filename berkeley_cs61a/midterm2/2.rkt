#lang simply-scheme
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define my-tree (make-tree 8 (make-tree 5 '() '())
                           (make-tree 12 '() '())))

(define (all-smaller? tree num) 
  (cond  ((null? tree) #t)
         ((> num (entry tree)) (and (all-smaller? (left-branch tree) num) (all-smaller? (right-branch tree) num)))
         ((< num (entry tree)) #f)
         (else #f))
  )

(define (all-larger? tree num) 
  (cond  ((null? tree) #t)
         ((< num (entry tree)) (and (all-larger? (left-branch tree) num) (all-larger? (right-branch tree) num)))
         ((> num (entry tree)) #f)
         (else #f))
  )


(all-smaller? my-tree 15) ;#t

(all-smaller? my-tree 10) ;#f

(all-larger? my-tree 1) ;#f

(all-larger? my-tree 10) ;#f


(define (bst? tree) 
  (cond  ((null? tree) #t)
         (else (and (all-smaller? (left-branch tree) (entry tree)) (all-larger? (right-branch tree) (entry tree))))
         ))

(bst? my-tree) ;#t

(define bst (make-tree 8 (make-tree 5  (make-tree 2 '() '()) (make-tree 4 '() '()))
                       (make-tree 12 (make-tree 11 '() '()) (make-tree 14 '() '()))))

(define not-bst (make-tree 8 (make-tree 9 '() '())
                           (make-tree 5 '() '())))

(bst? bst) ;#t

(bst? not-bst) ;#f