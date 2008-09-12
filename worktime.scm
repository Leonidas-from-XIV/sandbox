#lang scheme
(require scheme/cmdline)
(require srfi/19)
;(require rnrs/files-6)
;(require rnrs/io-simple-6)

;(define file-to-parse
;  (command-line
;   #:program "worktime"
;   #:args (filename)
;   filename))
(define file-to-parse "workdata.txt")

(define data-source (open-input-file file-to-parse))

; get the number of lines
(define port->list
  (lambda (port)
    (let ((line (read-line port)))
      (if (eof-object? line) '()
          (cons line (port->list port))))))

(define dates-list (port->list data-source))
; (make-date) from SRFI 19
; string-tokenize SRFI 13

;(regexp-split (regexp " ") (car dates-list))

(define generic-split
  (lambda (line first-delimiter first-index second-delimiter second-index)
    (let* ((first-chunk (list-ref (regexp-split (regexp first-delimiter) line) first-index))
           (element (list-ref (regexp-split (regexp second-delimiter) first-chunk) second-index)))
      (string->number element))))

(define line->day
  (lambda (line)
    (generic-split line " " 0 "\\." 0)))

(define line->month
  (lambda (line)
    (generic-split line " " 0 "\\." 1)))

(define line->year
  (lambda (line)
    (generic-split line " " 0 "\\." 2)))

(line->day (car dates-list))
(line->month (car dates-list))
(line->year (car dates-list))