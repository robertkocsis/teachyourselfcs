(define (make-time hr mn cat) (list hr mn cat))
(define hour car)
(define minute cadr)
(define category caddr)

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