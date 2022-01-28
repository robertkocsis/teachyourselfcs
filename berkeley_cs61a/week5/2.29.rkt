(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cadr mobile))


(define (branch-length branch) (car branch))

(define (branch-structure branch) (cadr branch))

(define (branch-weight branch)
  (if (number? (branch-structure branch))
      (branch-structure branch)
      (total-weight (branch-structure branch))
      )
  )

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define sub-mobile-a
  (make-mobile (make-branch 2 4) (make-branch 2 4))
  )

(define sub-mobile-b
  (make-mobile (make-branch 2 4) (make-branch 2 4))
  )

(define branch-a (make-branch 2 sub-mobile-a))

(define branch-b (make-branch 2 sub-mobile-b))

(define test-mobile (make-mobile branch-a branch-b))

(total-weight test-mobile)

(define (torque branch)
  (* (branch-length branch) (branch-weight branch))
  )
  
(define (balanced-branch? branch)
  (if (number? (branch-structure branch))
      #t
      (mobile-balanced? (branch-structure branch))  
      ))
  
(define (mobile-balanced? mobile)

  (let ((left-balanced (balanced-branch? (left-branch mobile)))
        (right-balanced (balanced-branch? (right-branch mobile))))
    (and (= (torque (left-branch mobile)) (torque (right-branch mobile))) left-balanced right-balanced)
    ))

(mobile-balanced? test-mobile)

