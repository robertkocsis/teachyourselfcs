(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (substitute items oldWord newWord)
  (define (subs word) (if (equal? word oldWord) newWord word))
  (define (deep-mapper x)
    (if (pair? x) (map deep-mapper x) (subs x))
    )
  (map deep-mapper items))

(substitute '((lead guitar) (bass guitar) (rhythm guitar) drums)
            'guitar 'axe)