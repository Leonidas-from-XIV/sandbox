#lang scheme
;;;; Simple garden fence encryption algorithm
;;;; <http://www.webplain.de/foren/read.php?1,8094>

;;; for DROP
(require srfi/1)

;;; returns every nth item of a list, starting with the first
(define every-nth-item
  (lambda (lat n)
    (cond [(empty? lat) '()]
          [(> n (length lat)) (list (car lat))]
          [else (cons (car lat)
                      (every-nth-item (drop lat n) n))])))

(define encode
  (lambda (chars times move)
    (cond [(= times 1) (every-nth-item chars move)]
          [else (cons (every-nth-item chars move)
                      (encode (cdr chars) (- times 1) move))])))

(encode (string->list "diesisteinklartext") 12 10)
(every-nth-item '(1 2 3 4) 5)