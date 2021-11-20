#lang scheme/base

;;; ============================================================================
;;; Port of the "Simply Scheme" book code to allow use under Racket
;;; ============================================================================
;;; It allows re-defining of module functions, which was the main obstacle
;;; to its operation previously.
;;; The first part is an adaptation of the necessary changes to run on Racket
;;; by importing aliased existing module functions using "require" and "only-in"
;;; The second part is the (slightly adapted) code from
;;; https://people.eecs.berkeley.edu/~bh/downloads/simply/simply.scm
;;; that implements the particular pedagogic requirements of Simply Scheme
;;; ----------------------------------------------------------------------------

;;; PART I

(require
  srfi/13
  ;; these are the ones that the module redefines
  ;; so they are pulled into it with aliased names
  (only-in scheme/base
           ; redefined for utility purposes in this file
           (equal? old-equal?)           
           (number->string old-number->string)
           (round old-round)
           (remainder old-remainder)
           (quotient old-quotient)
           ; redefined for "logoisation" in this file
           (* old-*)
           (+ old-+)
           (- old--)
           (/ old-/)
           (< old-<)
           (<= old-<=)
           (= old-=)
           (> old->)
           (>= old->=)
           (abs old-abs)
           (acos old-acos)
           (asin old-asin)
           (atan old-atan)
           (ceiling old-ceiling)
           (cos old-cos)
           (even? old-even?)
           (exp old-exp)
           (expt old-expt)
           (floor old-floor)
           (gcd old-gcd)
           (integer? old-integer?)
           (lcm old-lcm)
           (log old-log)
           (max old-max)
           (min old-min)
           (modulo old-modulo)
           (negative? old-negative?)
           (number? old-number?)
           (odd? old-odd?)
           (positive? old-positive?)
           (random old-random)
           (sin old-sin)
           (sqrt old-sqrt)
           (tan old-tan)
           (truncate old-truncate)
           (vector-ref old-vector-ref)
           (vector-set! old-vector-set!)
           (zero? old-zero?)
           (list-ref old-list-ref)
           (make-vector old-make-vector)
           )
  (only-in r5rs
           (set-cdr! set-cdr!)
           ; redefined for utility in this file
           (open-output-file old-open-output-file)
           (open-input-file old-open-input-file)
           (close-input-port old-close-input-port)
           (close-output-port old-close-output-port)
           )
  (only-in racket/trace
           (trace trace)
           (untrace untrace))
  )

; Then, set these temporarily to their "old", i.e. normal value but since
; they're now defined in this module we can set! their values as we like
; (i.e. to those used by SS for "logoization")
(define * old-*)
(define + old-+)
(define - old--)
(define / old-/)
(define < old-<)
(define <= old-<=)
(define = old-=)
(define > old->)
(define >= old->=)
(define abs old-abs)
(define acos old-acos)
(define asin old-asin)
(define atan old-atan)
(define ceiling old-ceiling)
(define cos old-cos)
(define even? old-even?)
(define exp old-exp)
(define expt old-expt)
(define floor old-floor)
(define gcd old-gcd)
(define integer? old-integer?)
(define lcm old-lcm)
(define list-ref old-list-ref)
(define log old-log)
(define max old-max)
(define min old-min)
(define modulo old-modulo)
(define odd?  old-odd?)
(define random  old-random)
(define sin  old-sin)
(define sqrt  old-sqrt)
(define tan  old-tan)
(define truncate old-truncate)
(define zero? old-zero?)
(define positive? old-positive?)
(define negative? old-negative?)
(define number? old-number?)
(define vector-ref old-vector-ref)
(define vector-set! old-vector-set!)
(define make-vector old-make-vector)
;(define round #f) ;     defined further down
;(define quotient #f) ;  defined further down
;(define remainder #f) ; defined further down


;;; PART II
;;; ====================================================

;;; simply.scm version 3.13 (8/11/98)
;;; (Numbered to agree with berkeley.scm version.)

;;; This file uses Scheme features we don't talk about in _Simply_Scheme_.
;;; Read at your own risk.

;; will define equal further on, just use this now for simplicity  ***AG***
(if (old-equal? 'foo (symbol->string 'foo)) 
    (error "Simply.scm already loaded!!")
    (display "Loading Simply Scheme...\n"))

;; if it's already a string just return it - ***AG***
(define number->string
  (λ args
    (if (string? (car args))
        (car args)
        (apply old-number->string args))))

;; Get strings in error messages to print nicely (especially "")
(define (whoops string . args)
  (define (error-printform x)
    (if (string? x)
        (string-append "\"" x "\"")
        x))
  (apply error (cons string (map error-printform args))))

;; ROUND returns an inexact integer if its argument is inexact,
;; but we think it should always return an exact integer.
(define (round  number)
  (inexact->exact (old-round number)))

;; Remainder and quotient blow up if their argument isn't an integer.
;; Unfortunately, in SCM, (* 365.25 24 60 60) *isn't* an integer.
(define (remainder x y)
  (old-remainder (if (integer? x) (inexact->exact x) x)
                 (if (integer? y) (inexact->exact y) y)))
(define (quotient x y)
  (old-quotient (if (integer? x) (inexact->exact x) x)
                (if (integer? y) (inexact->exact y) y)))

;; now set nemainder and quotient and round to new-remainder, etc
;; so they can be logoized appropriately
(define new-round #f)
(define new-remainder #f)
(define new-quotient #f)
(set! new-remainder remainder)
(set! new-quotient quotient)
(set! new-round round)

;; Supplied (random) omitted. Built-in "should" be better.
;; Make sure native behaves correctly for book code.

;;; Logo-style word/sentence implementation
(define (word? x)
  (or (symbol? x) (number? x) (string? x)))

(define sentence?
  (λ args
    (define (list-of-words? l)
      (cond ((null? l) #t)
            ((pair? l)
             (and (word? (car l)) (list-of-words? (cdr l))))
            (else #f)))
    (list-of-words? args)))

(define (empty? x)
  (or (null? x)
      (and (string? x) (string=? x ""))))

(define char-rank
  ;; 0 Letter in good case or special initial
  ;; 1 ., + or -
  ;; 2 Digit
  ;; adapted for modern Schemes, which don't capitalise symbols ***AG***
  ;; 3 Letter in upper case
  ;; 4 weird character
  (let ((*the-char-ranks* (make-vector 256 3)))  
    (define (rank-string str rank)
      (define (helper i len)
        (if (= i len)
            'done
            (begin (vector-set! *the-char-ranks*
                                (char->integer (string-ref str i))
                                rank)
                   (helper (+ i 1) len))))
      (helper 0 (string-length str)))
    (rank-string (symbol->string 'abcdefghijklmnopqrstuvwxyz) 0)
    (rank-string "!$%&*/:<=>?~_^" 0)
    (rank-string "+-." 1)
    (rank-string "0123456789" 2)
    (rank-string (symbol->string 'ABCDEFGHIJKLMNOPQRSTUVWXYZ) 4)
    (λ (char)		    ;; value of char-rank
      (vector-ref *the-char-ranks* (char->integer char)))))

(define string->word
  (λ (string)
    (define (subsequents? string i length)
      (cond ((= i length) #t)
            ((<= (char-rank (string-ref string i)) 2)
             (subsequents? string (+ i 1) length))
            (else #f)))
    (define (special-id? string)
      (or (string=? string "+")
          (string=? string "-")
          (string=? string "...")))
    (define (ok-symbol? string)
      (if (string=? string "")
          #f
          (let ((rank1 (char-rank (string-ref string 0))))
            (cond ((= rank1 0) (subsequents? string 1 (string-length string)))
                  ((= rank1 1) (special-id? string))
                  (else #f)))))
    (define (nn-helper string i len seen-point?)
      (cond ((= i len)
             (if seen-point?
                 (not (char=? (string-ref string (- len 1)) #\0))
                 #t))
            ((char=? #\. (string-ref string i))
             (cond (seen-point? #f)
                   ((= (+ i 2) len) #t)  ; Accepts "23.0"
                   (else (nn-helper string (+ i 1) len #t))))
            ((= 2 (char-rank (string-ref string i)))
             (nn-helper string (+ i 1) len seen-point?))
            (else #f)))
    (define (narrow-number? string)
      (if (string=? string "")
          #f
          (let* ((c0 (string-ref string 0))
                 (start 0)
                 (len (string-length string))
                 (cn (string-ref string (- len 1))))
            (if (and (char=? c0 #\-) (not (= len 1)))
                (begin
                  (set! start 1)
                  (set! c0 (string-ref string 1)))
                #f)
            (cond ((not (= (char-rank cn) 2)) #f)  ; Rejects "-" among others
                  ((char=? c0 #\.) #f)
                  ((char=? c0 #\0)
                   (cond ((= len 1) #t)  ; Accepts "0" but not "-0"
                         ((= len 2) #f)  ; Rejects "-0" and "03"
                         ((char=? (string-ref string (+ start 1)) #\.)
                          (nn-helper string (+ start 2) len #t))
                         (else #f)))
                  (else (nn-helper string start len #f))))))
    
    ;; The body of string->word:
    (cond ((narrow-number? string) (string->number string))
          ((ok-symbol? string) (string->symbol string))
          (else string))))

(define (char->word char)
  (let ((rank (char-rank char))
        (string (make-string 1 char)))
    (cond ((= rank 0) (string->symbol string))
          ((= rank 2) (string->number string))
          ((char=? char #\+) '+)
          ((char=? char #\-) '-)
          ;; this was not originally present,   ***AG***
          ;; since Berkeley Scheme capitalised output by default
          ((= rank 4) (string->symbol string))
          (else string))))

(define (word->string wd)
  (cond ((string? wd) wd)
        ((number? wd) (number->string wd))
        (else (symbol->string wd))))

(define (count stuff)
  (if (word? stuff)
      (string-length (word->string stuff))
      (length stuff)))

(define word
  (λ xs
    (string->word
     (apply string-append
            (map (λ (arg)
                   (if (word? arg)
                       (word->string arg)
                       (whoops "Invalid argument to WORD: " arg)))
                 xs)))))

(define se
  (let ()
    (define (paranoid-append a original-a b)
      (cond ((null? a) b)
            ((word? (car a))
             (cons (car a) (paranoid-append (cdr a) original-a b)))
            (else (whoops "Argument to SENTENCE not a word or sentence"
                          original-a ))))
    (define (combine-two a b)                ;; Note: b is always a list
      (cond ((pair? a) (paranoid-append a a b))
            ((null? a) b)
            ((word? a) (cons a b))
            (else (whoops "Argument to SENTENCE not a word or sentence:" a))))
    ;; Helper function so recursive calls don't show up in TRACE
    (define (real-se args)
      (if (null? args)
          '()
          (combine-two (car args) (real-se (cdr args)))))
    (λ args
      (real-se args))))
(define sentence se)

(define (first x)
  (define (word-first wd)
    (char->word (string-ref (word->string wd) 0)))
  (cond ((pair? x) (car x))
        ((empty? x) (whoops "Invalid argument to FIRST: " x))
        ((word? x) (word-first x))
        (else (whoops "Invalid argument to FIRST: " x))))

(define (last x)
  (define (word-last wd)
    (let ((s (word->string wd)))
      (char->word (string-ref s (- (string-length s) 1)))))
  (define (list-last lst)
    (if (empty? (cdr lst))
        (car lst)
        (list-last (cdr lst))))
  (cond ((pair? x) (list-last x))
        ((empty? x) (whoops "Invalid argument to LAST: " x))
        ((word? x) (word-last x))
        (else (whoops "Invalid argument to LAST: " x))))

(define (bf x)
  (define (string-bf s)
    (substring s 1 (string-length s)))
  (define (word-bf wd)
    (string->word (string-bf (word->string wd))))
  ; symbol-handling added for more modern Schemes ***AG***
  (define (symbol-bf wd)
    (string->symbol (string-bf (word->string wd))))  
  (cond ((pair? x) (cdr x))
        ((empty? x) (whoops "Invalid argument to BUTFIRST: " x))
        ((symbol? x) (symbol-bf x)) ; ***AG***
        ((word? x) (word-bf x))
        (else (whoops "Invalid argument to BUTFIRST: " x))))
(define butfirst bf)

(define (bl x)
  (define (list-bl list)
    (if (null? (cdr list))
        '()
        (cons (car list) (list-bl (cdr list)))))
  (define (string-bl s)
    (substring s 0 (- (string-length s) 1)))
  (define (word-bl wd)
    (string->word (string-bl (word->string wd))))
  ; symbol-handling added for more modern Schemes ***AG***
  (define (symbol-bl wd)
    (string->symbol (string-bl (word->string wd))))
  (cond ((pair? x) (list-bl x))
        ((empty? x) (whoops "Invalid argument to BUTLAST: " x))
        ((symbol? x) (symbol-bl x)) ; ***AG***
        ((word? x) (word-bl x))
        (else (whoops "Invalid argument to BUTLAST: " x))))
(define butlast bl)

(define (item n stuff)
  (define (word-item n wd)
    (char->word (string-ref (word->string wd) (- n 1))))
  (cond ((not (integer? n))
         (whoops "Invalid first argument to ITEM (must be an integer): "
                 n))
        ((< n 1)
         (whoops "Invalid first argument to ITEM (must be positive): "
                 n))
        ((> n (count stuff))
         (whoops "No such item: " n stuff))
        ((word? stuff) (word-item n stuff))
        ((list? stuff) (list-ref stuff (- n 1)))
        (else (whoops "Invalid second argument to ITEM: " stuff))))
;; the above function needs to be switchable for "logoization"
;; done like this until sure of define behaviour
(define old-item #f)
(set! old-item item)

(define equal?
  ;; Note that EQUAL? assumes strings are numbers.
  ;; (strings-are-numbers #f) doesn't change this behavior.
  (let ()
    (define (vector-equal? v1 v2)
      (let ((len1 (vector-length v1))
            (len2 (vector-length v2)))
        (define (helper i)
          (if (= i len1)
              #t
              (and (equal? (vector-ref v1 i) (vector-ref v2 i))
                   (helper (+ i 1)))))
        (if (= len1 len2)
            (helper 0)
            #f)))
    (λ (x y)
      (cond ((null? x) (null? y))
            ((null? y) #f)
            ((pair? x)
             (and (pair? y)
                  (equal? (car x) (car y))
                  (equal? (cdr x) (cdr y))))
            ((pair? y) #f)
            ((symbol? x)
             (or (and (symbol? y) (eq? x y))
                 (and (string? y) (string=? (symbol->string x) y))))
            ((symbol? y)
             (and (string? x) (string=? x (symbol->string y))))
            ((number? x)
             (or (and (number? y) (= x y))
                 (and (string? y)
                      (let ((possible-num (string->word y)))
                        (and (number? possible-num)
                             (= x possible-num))))))
            ((number? y)
             (and (string? x)
                  (let ((possible-num (string->word x)))
                    (and (number? possible-num)
                         (= possible-num y)))))
            ((string? x) (and (string? y) (string=? x y)))
            ((string? y) #f)
            ((vector? x) (and (vector? y) (vector-equal? x y)))
            ((vector? y) #f)
            (else (eqv? x y))))))

(define member?
  (let ()
    (define (symbol-in-list? symbol string lst)
      (cond ((null? lst) #f)
            ((and (symbol? (car lst))
                  (eq? symbol (car lst))))
            ((string? (car lst))
             (cond ((not string)
                    (symbol-in-list? symbol (symbol->string symbol) lst))
                   ((string=? string (car lst)) #t)
                   (else (symbol-in-list? symbol string (cdr lst)))))
            (else (symbol-in-list? symbol string (cdr lst)))))
    (define (word-in-list? wd lst)
      (cond ((null? lst) #f)
            ((equal? wd (car lst)) #t)
            (else (word-in-list? wd (cdr lst)))))
    (define (word-in-word? small big)
      (let ((one-letter-str (word->string small)))
        (if (> (string-length one-letter-str) 1)
            (whoops "Invalid arguments to MEMBER?: " small big)
            (let ((big-str (word->string big)))
              (char-in-string? (string-ref one-letter-str 0)
                               big-str
                               (- (string-length big-str) 1))))))
    (define (char-in-string? char string i)
      (cond ((< i 0) #f)
            ((char=? char (string-ref string i)) #t)
            (else (char-in-string? char string (- i 1)))))
    (λ (x stuff)
      (cond ((empty? stuff) #f)
            ((word? stuff) (word-in-word? x stuff))
            ((not (list? stuff))
             (whoops "Invalid second argument to MEMBER?: " stuff))
            ((symbol? x) (symbol-in-list? x #f stuff))
            ((or (number? x) (string? x))
             (word-in-list? x stuff))
            (else (whoops "Invalid first argument to MEMBER?: " x))))))

(define (before? wd1 wd2)
  (cond ((not (word? wd1))
         (whoops "Invalid first argument to BEFORE? (not a word): " wd1))
        ((not (word? wd2))
         (whoops "Invalid second argument to BEFORE? (not a word): " wd2))
        (else (string<? (word->string wd1) (word->string wd2)))))


;;; Higher Order Functions

(define (filter pred l)
  ;; Helper function so recursive calls don't show up in TRACE
  (define (real-filter l)
    (cond ((null? l) '())
          ((pred (car l))
           (cons (car l) (real-filter (cdr l))))
          (else (real-filter (cdr l)))))
  (cond ((not (procedure? pred))
         (whoops "Invalid first argument to FILTER (not a procedure): "
                 pred))
        ((not (list? l))
         (whoops "Invalid second argument to FILTER (not a list): " l))
        (else (real-filter l))))

(define (keep pred w-or-s)
  (define (keep-string in i out out-len len)
    (cond ((= i len) (substring out 0 out-len))
          ((pred (char->word (string-ref in i)))
           (string-set! out out-len (string-ref in i))
           (keep-string in (+ i 1) out (+ out-len 1) len))
          (else (keep-string in (+ i 1) out out-len len))))
  (define (keep-word wd)
    (let* ((string (word->string wd))
           (len (string-length string)))
      (string->word
       (keep-string string 0 (make-string len) 0 len))))
  (cond ((not (procedure? pred))
         (whoops "Invalid first argument to KEEP (not a procedure): "
                 pred))
        ((pair? w-or-s) (filter pred w-or-s))
        ((word? w-or-s) (keep-word w-or-s))
        ((null? w-or-s) '())
        (else
         (whoops "Bad second argument to KEEP (not a word or sentence): "
                 w-or-s))))

(define (appearances item aggregate)
  (count (keep (λ (element) (equal? item element)) aggregate)))

(define (every fn stuff)
  (define (string-every string i length)
    (if (= i length)
        '()
        (se (fn (char->word (string-ref string i)))
            (string-every string (+ i 1) length))))
  (define (sent-every sent)
    ;; This proc. can't be optimized or else it will break the
    ;; exercise where we ask them to reimplement sentences as
    ;; vectors and then see if every still works.
    (if (empty? sent)
        sent		; Can't be '() or exercise breaks.
        (se (fn (first sent))    
            (sent-every (bf sent)))))
  (cond ((not (procedure? fn))
         (whoops "Invalid first argument to EVERY (not a procedure):"
                 fn))
        ((word? stuff)
         (let ((string (word->string stuff)))
           (string-every string 0 (string-length string))))
        (else (sent-every stuff))))

(define (accumulate combiner stuff)
  (define (real-accumulate stuff)
    (if (empty? (bf stuff))
        (first stuff)
        (combiner (first stuff) (real-accumulate (bf stuff)))))
  (cond ((not (procedure? combiner))
         (whoops "Invalid first argument to ACCUMULATE (not a procedure):"
                 combiner))
        ((not (empty? stuff)) (real-accumulate stuff))
        ((member combiner (list + * word se)) (combiner))
        (else (whoops "Can't accumulate empty input with that combiner"))))

(define (reduce combiner stuff)
  (define (real-reduce stuff)
    (if (null? (cdr stuff))
        (car stuff)
        (combiner (car stuff) (real-reduce (cdr stuff)))))
  (cond ((not (procedure? combiner))
         (whoops "Invalid first argument to REDUCE (not a procedure):"
                 combiner))
        ((not (null? stuff)) (real-reduce stuff))
        ((member combiner (list + * word se append)) (combiner))
        (else (whoops "Can't reduce empty input with that combiner"))))

(define ((repeated fn number) x)
  (if (= number 0) x ((repeated fn (- number 1)) (fn x))))
;; the above function needs to be switchable for "logoization"
;; done like this until sure of define behaviour
(define old-repeated #f)
(set! old-repeated repeated)

;; Tree stuff
(define make-node cons)
(define datum car)
(define children cdr)


;; I/O

(define show
  (λ args
    (cond
      ((= (length args) 1)
       (display (car args))
       (newline))
      ((= (length args) 2)
       (if (not (output-port? (car (cdr args))))
           (whoops "Invalid second argument to SHOW (not an output port): "
                   (car (cdr args)))
           #t) ;; no if alternative previously!  ***AG***
       (apply display args)
       (newline (car (cdr args))))
      (else (whoops "Incorrect number of arguments to procedure SHOW")))))

(define (show-line  line . args)
  (if (>= (length args) 2)
      (whoops "Too many arguments to show-line")
      (let ((port (if (null? args) (current-output-port) (car args))))
        (cond ((not (list? line))
               (whoops "Invalid argument to SHOW-LINE (not a list):" line))
              ((null? line) #f)
              (else
               (display (car line) port)
               (for-each (λ (wd) (display " " port) (display wd port))
                         (cdr line))))
        (newline port))))

(define read-string
  (let ()
    (define (read-string-helper chars all-length chunk-length port)
      (let ((char (read-char port))
            (string (car chars)))
        (cond ((or (eof-object? char) (eqv? char #\newline))
               (apply string-append
                      (reverse
                       (cons
                        (substring (car chars) 0 chunk-length)
                        (cdr chars)))))
              ((>= chunk-length 80)
               (let ((newstring (make-string 80)))
                 (string-set! newstring 0 char)
                 (read-string-helper (cons newstring chars)
                                     (+ all-length 1)
                                     1
                                     port)))
              (else
               (string-set! string chunk-length char)
               (read-string-helper chars
                                   (+ all-length 1)
                                   (+ chunk-length 1)
                                   port)))))
    (λ args
      (if (>= (length args) 2)
          (whoops "Too many arguments to read-string")
          (let ((port (if (null? args) (current-input-port) (car args))))
            (if (eof-object? (peek-char port))
                (read-char port)
                (read-string-helper (list (make-string 80)) 0 0 port)))))))

(define read-line
  (let ((= =)
        (list list)
        (string->word string->word)
        (substring substring)
        (char-whitespace? char-whitespace?)
        (string-ref string-ref)
        (+ +)
        (string-length string-length)
        (apply apply)
        (read-string read-string))
    (λ args
      (define (tokenize string)
        (define (helper i start len)
          (cond ((= i len)
                 (if (= i start)
                     '()
                     (list (string->word (substring string start i)))))
                ((char-whitespace? (string-ref string i))
                 (if (= i start)
                     (helper (+ i 1) (+ i 1) len)
                     (cons (string->word (substring string start i))
                           (helper (+ i 1) (+ i 1) len))))
                (else (helper (+ i 1) start len))))
        (if (eof-object? string)
            string
            (helper 0 0 (string-length string))))
      (tokenize (apply read-string args)))))

(define *the-open-inports* '())
(define *the-open-outports* '())

(define align
  (let ((< <) (abs abs) (* *) (expt expt) (>= >=) (- -) (+ +) (= =)
              (null? null?)
              (car car)
              (round round)
              (number->string number->string)
              (string-length string-length)
              (string-append string-append)
              (make-string make-string)
              (substring substring)
              (string-set! string-set!)
              (number? number?)
              (word->string word->string))
    (λ (obj width . rest)
      (define (align-number obj width rest)
        (let* ((sign (< obj 0))
               (num (abs obj))
               (prec (if (null? rest) 0 (car rest)))
               (big (round (* num (expt 10 prec))))
               (cvt0 (number->string big))
               (cvt (if (< num 1) (string-append "0" cvt0) cvt0))
               (pos-str (if (>= (string-length cvt0) prec)
                            cvt
                            (string-append
                             (make-string (- prec (string-length cvt0)) #\0)
                             cvt)))
               (string (if sign (string-append "-" pos-str) pos-str))
               (length (+ (string-length string)
                          (if (= prec 0) 0 1)))
               (left (- length (+ 1 prec)))
               (result (if (= prec 0)
                           string
                           (string-append
                            (substring string 0 left)
                            "."
                            (substring string left (- length 1))))))
          (cond ((= length width) result)
                ((< length width)
                 (string-append (make-string (- width length) #\space) result))
                (else (let ((new (substring result 0 width)))
                        (string-set! new (- width 1) #\+)
                        new)))))
      (define (align-word string)
        (let ((length (string-length string)))
          (cond ((= length width) string)
                ((< length width)
                 (string-append string (make-string (- width length) #\space)))
                (else (let ((new (substring string 0 width)))
                        (string-set! new (- width 1) #\+)
                        new)))))
      (if (number? obj)
          (align-number obj width rest)
          (align-word (word->string obj))))))
;; the above function needs to be switchable for "logoization"
;; done like this until sure of define behaviour
(define old-align #f)
(set! old-align align)

(define open-output-file
  (let ((oof old-open-output-file))
    (λ (filename)
      (let ((port (oof filename)))
        (set! *the-open-outports* (cons port *the-open-outports*))
        port))))

(define open-input-file
  (let ((oif old-open-input-file)
        (cons cons))
    (λ (filename)
      (let ((port (oif filename)))
        (set! *the-open-inports* (cons port *the-open-inports*))
        port))))

(define (remove! thing lst)
  (define (r! prev)
    (cond ((null? (cdr prev)) lst)
          ((eq? thing (car (cdr prev)))
           (set-cdr! prev (cdr (cdr prev)))
           lst)
          (else (r! (cdr prev)))))
  (cond ((null? lst) lst)
        ((eq? thing (car lst)) (cdr lst))
        (else (r! lst))))

(define close-input-port
  (let ((cip old-close-input-port)
        (remove! remove!))
    (λ (port)
      (set! *the-open-inports* (remove! port *the-open-inports*))
      (cip port))))

(define close-output-port
  (let ((cop old-close-output-port))
    (λ (port)
      (set! *the-open-outports* (remove! port *the-open-outports*))
      (cop port))))

(define (close-all-ports)
  (for-each close-input-port *the-open-inports*)
  (for-each close-output-port *the-open-outports*)
  'closed)

;; Make arithmetic work on numbers in string form:
(define (maybe-num arg)
  (if (string? arg)
      (let ((num (string->number arg)))
        (if num num arg))
      arg))

(define (logoize fn)
  (λ args
    (apply fn (map maybe-num args))))

;; special case versions of logoize, since (lambda args ...) is expensive
(define ((logoize-1 fn) x) (fn (maybe-num x)))
(define ((logoize-2 fn) x y) (fn (maybe-num x) (maybe-num y)))

;; If strings /are/ numbers, then "logoise" them
(define strings-are-numbers
  (let ((are-they? #f))
    (λ (yesno)
      (cond ((and are-they? (eq? yesno #t))
             (show "Strings are already numbers"))
            ((eq? yesno #t)
             (set! are-they? #t)
             (set! * (logoize old-*))
             (set! + (logoize old-+))
             (set! - (logoize old--))
             (set! / (logoize old-/))
             (set! < (logoize old-<))
             (set! <= (logoize old-<=))
             (set! = (logoize old-=))
             (set! > (logoize old->))
             (set! >= (logoize old->=))
             (set! abs (logoize-1 old-abs))
             (set! acos (logoize-1 old-acos))
             (set! asin (logoize-1 old-asin))
             (set! atan (logoize old-atan))
             (set! ceiling (logoize-1 old-ceiling))
             (set! cos (logoize-1 old-cos))
             (set! even? (logoize-1 old-even?))
             (set! exp (logoize-1 old-exp))
             (set! expt (logoize-2 old-expt))
             (set! floor (logoize-1 old-floor))
             (set! gcd (logoize old-gcd))
             (set! integer? (logoize-1 old-integer?))
             (set! lcm (logoize old-lcm))
             (set! log (logoize-1 old-log))
             (set! max (logoize old-max))
             (set! min (logoize old-min))
             (set! modulo (logoize-2 old-modulo))
             (set! negative? (logoize-1 old-negative?))
             (set! number? (logoize-1 old-number?))
             (set! odd? (logoize-1 old-odd?))
             (set! positive? (logoize-1 old-positive?))
             (set! quotient (logoize-2 new-quotient))
             (set! random (logoize old-random))
             (set! remainder (logoize-2 new-remainder))
             (set! round (logoize-1 new-round))
             (set! sin (logoize-1 old-sin))
             (set! sqrt (logoize-1 old-sqrt))             
             (set! tan (logoize-1 old-tan))
             (set! truncate (logoize-1 old-truncate))
             (set! zero? (logoize-1 old-zero?))
             (set! vector-ref
                   (λ (vec i) (old-vector-ref vec (maybe-num i))))
             (set! vector-set!
                   (λ (vec i val)
                     (old-vector-set! vec (maybe-num i) val)))
             (set! make-vector
                   (λ (num . args)
                     (apply old-make-vector
                            (cons (maybe-num num) args))))
             (set! list-ref
                   (λ (lst i) (old-list-ref lst (maybe-num i))))
             ; program defined:
             (set! align (logoize align))
             (set! item (λ (n stuff) (old-item (maybe-num n) stuff)))
             (set! repeated  (λ (fn n) (old-repeated fn (maybe-num n))))
             )
            ((and (not are-they?) (not yesno))
             (show "Strings are already not numbers"))
            ((not yesno)
             (set! are-they? #f)
             (set! * old-*)
             (set! + old-+)
             (set! - old--)
             (set! / old-/)
             (set! < old-<)
             (set! <= old-<=)
             (set! = old-=)
             (set! > old->)
             (set! >= old->=)
             (set! abs old-abs)
             (set! acos old-acos)
             (set! asin old-asin)
             (set! atan old-atan)
             (set! ceiling old-ceiling)
             (set! cos old-cos)
             (set! even? old-even?)
             (set! exp old-exp)
             (set! expt old-expt)
             (set! floor old-floor)
             (set! gcd old-gcd)
             (set! integer? old-integer?)
             (set! lcm old-lcm)
             (set! list-ref old-list-ref)
             (set! log old-log)
             (set! max old-max)
             (set! min old-min)
             (set! modulo old-modulo)
             (set! odd? old-odd?)
             (set! quotient new-quotient)
             (set! random old-random)
             (set! remainder new-remainder)
             (set! round new-round)
             (set! sin old-sin)
             (set! sqrt old-sqrt)
             (set! tan old-tan)
             (set! truncate old-truncate)
             (set! zero? old-zero?)
             (set! positive? old-positive?)
             (set! negative? old-negative?)
             (set! number? old-number?)
             (set! vector-ref old-vector-ref)
             (set! vector-set! old-vector-set!)
             (set! make-vector old-make-vector)
             (set! list-ref old-list-ref)
             ; program defined:
             (set! align old-align)
             (set! item old-item)
             (set! repeated old-repeated)
             )
            (else (whoops "Strings-are-numbers: give a #t or a #f")))
      are-they?)))

;; By default, strings are numbers:
(if
 (strings-are-numbers #t)
 (display "All done.\n") #f)

(provide (all-defined-out))
(provide (all-from-out scheme/base))
(provide trace untrace)

#|
(provide
 (except-out
  (all-from-out scheme/base)
  ;; scheme/base
  old-equal? old-number->string old-round old-remainder old-quotient
  old-* old-+ old-- old-/ old-< old-<= old-= old-> old->=
  old-abs old-acos old-asin old-atan old-ceiling old-cos
  old-even? old-exp old-expt old-floor old-gcd old-integer? old-lcm old-log
  old-max old-min old-modulo old-negative? old-number? old-odd? old-positive?
  old-random old-sin old-sqrt old-tan old-truncate
  old-vector-ref old-vector-set! old-zero? old-list-ref old-make-vector))
|#