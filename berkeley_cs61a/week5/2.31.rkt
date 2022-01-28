(define (tree-map fn tree)
  (cond ((null? tree) '())
        ((number? tree) (fn tree))
        (else (cons (tree-map fn (car tree))
                    (tree-map fn (cdr tree))))
        )
  )

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))