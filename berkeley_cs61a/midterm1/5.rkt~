(define (vowel? letter)
  (member? letter '(a e i o u)))

(define (syllables word)
  (cond 
    ((empty? word) 0)
    ((and (vowel? (first word)) (empty? (bf word))) 1)
    ((and (vowel? (first word)) (vowel? (first (bf word)))) (+ 0 (syllables (bf word))))
    ((and (vowel? (first word)) (not (vowel? (first (bf word))))) (+ 1 (syllables (bf word))))
    (else (syllables (bf word)))
         
    )
  )

(syllables 'banana)

(syllables 'aardvark)

(syllables 'cloud)