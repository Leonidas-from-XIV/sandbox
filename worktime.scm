#lang scheme
;; Program which calculates working hours
; command line parser
(require scheme/cmdline)
; datetime handling
(require srfi/19)
; string-tokenize
(require srfi/13)
; charset for the tokenizer
(require srfi/14)

; start-time has to be some very early date, epoch 0 at best
(define start-time (make-parameter (make-time time-utc 0 0)))
; end-time has is the current-time
(define end-time (make-parameter (current-time)))
; default filename
(define file-to-parse (make-parameter "workdata.txt"))

; reads lines until eof and returns a list
(define port->list
  (lambda (port)
    (let ((line (read-line port)))
      (if (eof-object? line) '()
          (cons line (port->list port))))))

(define generic-split-ref
  (lambda (line ref)
    (string->number 
     (list-ref 
      (string-tokenize line
                       ; split by :-. and space
                       (char-set-delete char-set:full #\: #\ #\- #\.))
      ref))))

(define line->day
  (lambda (line)
    (generic-split-ref line 0)))

(define line->month
  (lambda (line)
    (generic-split-ref line 1)))

(define line->year
  (lambda (line)
    (let [(input-year (generic-split-ref line 2))]
      ; only add 2000 if the year number is smaller than 100
      (if (< input-year 100)
          (+ 2000 input-year)
          input-year))))

(define line->start-hour
  (lambda (line)
    (generic-split-ref line 3)))

(define line->start-minute
  (lambda (line)
    (generic-split-ref line 4)))

(define line->end-hour
  (lambda (line)
    (generic-split-ref line 5)))

(define line->end-minute
  (lambda (line)
    (generic-split-ref line 6)))

(define line->date
  (lambda (line hour-accessor minute-accessor)
    (make-date 0 0
               (minute-accessor line)
               (hour-accessor line)
               (line->day line)
               (line->month line)
               (line->year line)
               0)))

(define line->start-date
  (lambda (line)
    (line->date line line->start-hour line->start-minute)))

(define line->end-date
  (lambda (line)
    (line->date line line->end-hour line->end-minute)))

(define time-hours
  (lambda (time)
    (/ (time-second time) 3600)))

(command-line
 #:program "worktime"
 #:once-each
 [("-s" "--start") start-date "Date to start calculation"
                   (start-time (date->time-utc
                                (make-date 0 0 0 0
                                           (line->day start-date)
                                           (line->month start-date)
                                           (line->year start-date)
                                           0)))]
 [("-e" "--end") end-date "Date to end calculation"
                 (end-time (date->time-utc
                            ; last second of the last hour of the last day
                            (make-date 0 59 59 23
                                       (line->day end-date)
                                       (line->month end-date)
                                       (line->year end-date)
                                       0)))]
 [("-f" "--file") file "File to use for calculation"
                  (file-to-parse file)])

; open the file
(define input-data (port->list (open-input-file (file-to-parse))))

; filter out all lines which are excluded by the boundaries
(define applicable-data
  (filter (lambda (line)
            (let [(current (date->time-utc
                            (line->start-date line)))]
              ; start-time <= current <= end-time
              (and (time>=? current (start-time))
                   (time<=? current (end-time)))))
          input-data))

; subtract each start from end date
(define hours-worked
  (map (lambda (line)
         (let ((start (date->time-utc (line->start-date line)))
               (stop (date->time-utc (line->end-date line))))
           (time-hours (time-difference stop start))))
       applicable-data))

; add them all together
(apply + hours-worked)