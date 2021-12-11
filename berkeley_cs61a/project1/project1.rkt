(define (point-of-card card)
  (cond ((or (equal? (first card) 'J) (equal? (first card) 'Q) (equal? (first card) 'K)) 10)
        ((or (equal? (first card) 'A) (equal? (first card) 'a)) 11)
        (else (+ 0 (bl card))))
  )

(define (best-total sentence)
  (define (max-total sentence)
    (if (empty? sentence) 0 (+ (point-of-card (first sentence)) (max-total (bf sentence))))
    )

  (define (num-of-eleven-aces sentence)
    (define (isAce? x)
      (if (= 11 (point-of-card (first sentence))) 1 0)
      )

    
    (if (empty? sentence) 0
        (+ ((lambda (x)
              (isAce? x)
              ) (first sentence))  (num-of-eleven-aces (bf sentence)))
        )
    )
  
  (define (iter total eleven-ace-count)
    (if (and (> total 21) (> eleven-ace-count 0)) (iter (- total 10) (- eleven-ace-count 1)) total)
    )
  
  (iter (max-total sentence) (num-of-eleven-aces sentence))
  )

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(best-total '(ad 8s))
(best-total '(ad 8s 5h))
(best-total '(ad as 9h))


(define (play-n strategy n)
  (if (= n 0) 0 (+ (twenty-one strategy) (play-n strategy (- n 1)) )))

(define (stop-at n)
  (lambda (customer-hand dealer-up-card)
    
     (if (< (best-total customer-hand) (+ n 1)) #t #f)
     ))

(define (stop-at-17 customer-hand dealer-up-card)
  ((stop-at 17) customer-hand dealer-up-card)
  )

(define (dealer-sensitive customer-hand dealer-up-card)
  (cond ((and
         (> (point-of-card dealer-up-card) 6)
         ((stop-at 17) customer-hand dealer-up-card)
         )
         #t
         )
        ((and
          (< (point-of-card dealer-up-card) 7)
          ((stop-at 12) customer-hand dealer-up-card)
          )
         #t)
        (else #f)
        )
  )

(define (valentine customer-hand dealer-up-card)
  (define (hasHeart? sentence)
    (cond ((empty? sentence) #f)
          ((equal? (last (first sentence)) 'h) #t)
          (else (hasHeart? (bf sentence)))
          )
    )

  (if (hasHeart? customer-hand) ((stop-at 19) customer-hand dealer-up-card) ((stop-at 17) customer-hand dealer-up-card))
  )

(define (suit-strategy suit inc not-inc)
  (define (hasSuit? sentence)
    (cond ((empty? sentence) #f)
          ((equal? (last (first sentence)) suit) #t)
          (else (hasSuit? (bf sentence)))
          )
    )
  
  (lambda (customer-hand dealer-up-card)
  (if (hasSuit? customer-hand)
      (inc customer-hand dealer-up-card)
      (not-inc customer-hand dealer-up-card)
      )
  ))


(define (majority customer-hand dealer-up-card)
  (if (or
       (and ((stop-at 17) customer-hand dealer-up-card) (dealer-sensitive customer-hand dealer-up-card))
       (and ((stop-at 17) customer-hand dealer-up-card) (valentine customer-hand dealer-up-card))
       (and (dealer-sensitive customer-hand dealer-up-card) (valentine customer-hand dealer-up-card))
       )
      #t
      #f
  )
  )

(define (reckless strategy)
  (lambda (customer-hand dealer-up-card)
    (
     if (strategy (bf customer-hand) dealer-up-card)
        #t
        #f
     )

    )
  )

(play-n stop-at-17 50)

(play-n dealer-sensitive 50)

(play-n (stop-at 17) 50)

(play-n valentine 50)

(play-n (suit-strategy 'h (stop-at 19) (stop-at 17)) 50)

(play-n majority 50)

(play-n (reckless majority) 50)
