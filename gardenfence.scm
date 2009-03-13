#lang scheme
;;;; Simple garden fence encryption algorithm
;;;; <http://www.webplain.de/foren/read.php?1,8094>

;;; for DROP
(require srfi/1)

;;; returns every nth item of a list, starting with the first
;;; but don't pass values of n larger than
(define every-nth-item
  (lambda (lat n)
    (if (empty? lat) '()
        (cons (car lat) 
              (every-nth-item (drop lat n) n)))))

(define encode
  (lambda (text depth)
    (let [(chars (string->list text))]
      (every-nth-item chars 2))))

(encode "diesisteinklartext" 6)
(every-nth-item '(1 2 3 4) 2)