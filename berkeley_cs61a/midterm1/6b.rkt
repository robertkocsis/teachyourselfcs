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

(define (order-checker func)
  (lambda (sentence) (in-order? func sentence))
  )

(define length-ordered? (order-checker shorter?))

(length-ordered? '(i saw them standing togethers))

(length-ordered? '(i saw her standing there))

((order-checker <) '(2 3 5 5 8 13))

((order-checker <=) '(2 3 5 5 8 13))

((order-checker >) '(23 14 7 5 2))