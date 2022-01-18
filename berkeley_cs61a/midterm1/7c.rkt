

(define (make-time hr mn cat)
  (list
   ((lambda () (if (equal? cat 'pm) (+ 12 hr) hr)))
   mn)
  )
(define hour (lambda (time) (if (< (car time) 12) (car time) (- (car time) 12))))
(define minute cadr)
(define category (lambda (time) (if (< (car time) 12) 'am 'pm)))

(define (min-representation minute)
  (if (< minute 10) (word '0 minute) minute))

(define (time-print-form time)
  (display (hour time))
  (display ':)
  (display (min-representation (minute time)))
  (display (category time)) (newline)
  )

(time-print-form (make-time 3 7 'pm))

(define (24-hour time)
  (if (equal? (category time) 'pm) (+ (* (+ 12 (hour time)) 100) (minute time))
      (+ (hour time) (minute time))
      ))

(24-hour (make-time 3 47 'pm))