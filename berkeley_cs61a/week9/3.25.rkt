#lang sicp

;(define (assoc key records)
;(cond ((null? records) #f)
;((equal? key (caar records)) (car records))
;(else (assoc key (cdr records))) ))

(define (lookup keylist table)
  (cond ((not table) #f)
        ((null? keylist) (cdr table))
        (else (lookup (cdr keylist)
                      (assoc (car keylist) (cdr table))))))

(define (insert! keylist value table)
  (if (null? keylist)
      (set-cdr! table value)
      (let ((record (assoc (car keylist) (cdr table))))
        (if (not record)
            (begin
              (set-cdr! table
                        (cons (list (car keylist)) (cdr table)))
              (insert! (cdr keylist) value (cadr table)))
            (insert! (cdr keylist) value record)))))