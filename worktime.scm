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

; start-date has to be some very early date, epoch 0 at best
(define start-date (make-parameter #f))
; end-date has to be the maximum date
(define end-date (make-parameter #f))
; default filename
(define file-to-parse (make-parameter "workdata.txt"))

(command-line
 #:program "worktime"
 #:once-each
 (("-s" "--start") startdate "Date to start calculation"
                   (start-date startdate))
 (("-e" "--end") enddate "Date to end calculation"
                 (end-date enddate))
 (("-f" "--file") file "File to use for calculation"
                  (file-to-parse file)))

(define data-source (open-input-file (file-to-parse)))

; reads lines until eof and returns a list
(define port->list
  (lambda (port)
    (let ((line (read-line port)))
      (if (eof-object? line) '()
          (cons line (port->list port))))))

(define dates-list (port->list data-source))

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
    (+ 2000 (generic-split-ref line 2))))

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

(define start (date->time-utc (line->start-date (car dates-list))))
(define stop (date->time-utc (line->end-date (car dates-list))))
(define diff (time-difference stop start))
(time-hours diff)