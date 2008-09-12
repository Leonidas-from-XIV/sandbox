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
(define line->day
  (lambda (line)
    (let* ((date (car (regexp-split (regexp " ") line)))
           (day (list-ref (regexp-split (regexp "\\.") date) 0)))
      (string->number day))))

(define line->month
  (lambda (line)
    (let* ((date (car (regexp-split (regexp " ") line)))
           (day (list-ref (regexp-split (regexp "\\.") date) 1)))
      (string->number day))))

(define line->year
  (lambda (line)
    (let* ((date (car (regexp-split (regexp " ") line)))
           (day (list-ref (regexp-split (regexp "\\.") date) 2)))
      (string->number day))))

(line->day (car dates-list))
(line->month (car dates-list))
(line->year (car dates-list))