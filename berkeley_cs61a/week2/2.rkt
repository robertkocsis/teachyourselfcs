(define (customEvery fn sentence)
  (if (empty? sentence)
      '()
      (se (fn (first sentence)) (customEvery fn (bf sentence)))))

(define (square x) (* x x))

(customEvery square '(1 2 3 4))

(customEvery first '(nowhere man))

(every square '(1 2 3 4))