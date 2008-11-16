#lang scheme
(define merge
  (lambda (s r)
    (cond [(empty? s) r]
          [(empty? r) s]
          [else 
           (if (>= (car s) (car r))
               (cons (car s) (merge (cdr s) r))
               (cons (car r) (merge s (cdr r))))])))

(define s '(1 3 5))
(define r '(2 4 6))

(merge s r)