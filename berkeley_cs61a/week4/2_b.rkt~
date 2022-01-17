(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (get-at-index items index)
  (define (iterate elements i)
    (cond ((empty? elements) -1)
          ((equal? index i) (first elements))
          (else (iterate (bf elements) (+ i 1))))
    )
  (iterate items 0)
  )

(define (get-index items element) 
  (define (iterate elements i)
    (cond ((empty? elements) -1)
          ((equal? element (first elements)) i)
          (else (iterate (bf elements) (+ i 1))))
    )
  (iterate items 0)
  )

(define (substitute2 items oldWords newWords)
  (define (subs word)
    (if (member word oldWords) (get-at-index newWords (get-index oldWords word)) word))


  
  (define (deep-mapper x)
    (if (pair? x) (map deep-mapper x) (subs x))
    )
  (map deep-mapper items))

(get-at-index '(1 2 3 4 5) 4)
(get-at-index '(1 2 3 4 5) 5)

(get-index '(1 2 3 4 5) 5)
(get-index '(1 2 3 4 5) 6)

(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
             '(1 2 3 4) '(one two three four))