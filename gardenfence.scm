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

;;; bad name, this does not actually encode
(define encode
  (lambda (chars times move)
    (cond [(= times 1) '()]
          [else (cons (every-nth-item chars move)
                      (encode (cdr chars) (- times 1) move))])))

(define merge-phases
  (lambda (lat)
    (let* ([len (length lat)]
           [head (car lat)]
           ; last is wrong
           [tail (last lat)])
      `(,head ,@(merge-all-double lat 1 (- len 1)) ,tail))))

(define merge-double-phases
  (lambda (lat first second)
    (let ([head (list-ref lat first)]
          [tail (list-ref lat second)])
      (list head tail))))

(define merge-all-double
  (lambda (lat begin end)
    (if (>= begin end) '()
        (cons (merge-double-phases lat begin end)
              (merge-all-double lat (+ begin 1) (- end 1))))))

;; 11 = (height * 2) - 1
(define a (encode (string->list "diesisteinklartext") 11 10))
a
;(every-nth-item '(1 2 3 4) 5)
(merge-phases a)