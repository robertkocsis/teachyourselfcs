(define (in-order? func sen)
  (define (iterator items answer)
    (cond ((empty? items) answer)
          ( (empty? (bf items)) answer)
          ((func (first items) (first (bf items))) (iterator (bf items) #t))
          (else #f)
          ))
  (iterator sen #t)
  )

(define (shorter? a b)
  (< (count a) (count b)) )

(in-order? shorter? '(i saw them standing togethers))

(in-order? shorter? '(i saw her standing there))

(in-order? < '(2 3 5 5 8 13))

(in-order? <= '(2 3 5 5 8 13))

(in-order? > '(23 14 7 5 2))