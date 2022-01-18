>(every - (keep number? '(the 1 after 909)))
'(-1 -909)

> ((lambda (a b) ((if (< b a) + *) b a)) 4 6)
24

> (word (first '(cat)) (butlast 'dog))
'catdo

> (cons (list 1 2) (cons 3 4))
'((1 2) 3 . 4)

> (let ((p (list 4 5)))
(cons (cdr p) (cddr p)) )
'((5))

> (cadadr '((a (b) c) (d (e) f) (g (h) i)))
'(e)