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
           ; 5 = height - 1
           [tail (list-ref lat 5)]
           ; merge all items except for the first and last
           [grouped (merge-all-double lat 1 (- len 1))]
           [merged (map (lambda (arg) (apply interweave arg)) grouped)])
      `(,head ,@merged ,tail))))

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

;;; (a b c) (d e f) -> (a d b e c f)
(define interweave
  (lambda (head-lat tail-lat)
    (cond [(null? head-lat) tail-lat]
          [(null? tail-lat) head-lat]
          [else ;; (first-of-head first-of-tail recursive-rest)
           `(,(car head-lat) ,(car tail-lat) 
                             ,@(interweave (cdr head-lat) (cdr tail-lat)))])))

;;; flattens a list of lists of chars into a string
(define flatten
  (lambda (lat)
    (apply string-append (map list->string lat))))

;; 11 = (height * 2) - 1
(define a (encode (string->list "diesisteinklartext") 11 10))
;(every-nth-item '(1 2 3 4) 5)
(define b (merge-phases a))
(define c (flatten b))

;;; decode
(define decode
  (lambda (text height)
    (foldl
     ;; the function to apply on each letter
     (lambda (letter lat) (display letter))
     ;; initial value: n (height) empty lists
     (build-list height (lambda (e) '()))
     ;; the value to be folded
     (enumerate-cycle (string->list text) height))))

;;; like enumerate in Python
(define enumerate
  (lambda (lat)
    (let ([indices (build-list (length lat) values)])
      (zip indices lat))))

;;; like enumerate but cycles the argument
(define enumerate-cycle
  (lambda (lat size)
    ;; calculate the modulo size of all indizes
    ;; curry creates a partial function
    (let ([indices (map (curryr modulo size) (build-list (length lat) values))])
      (zip indices lat))))

(decode c 6)