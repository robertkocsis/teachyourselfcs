#lang simply-scheme

;(map caddr '((2 3 5) (7 11 13) (17 19)))  error because the third element has only two elements inside

(list (cons 2 (cons 3 5)))

(append (list '(2) '(3)) (cons '(4) '(5)))

(list (cons '(0) '(1)) (append '(2) '(3)))