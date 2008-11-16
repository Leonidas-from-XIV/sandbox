#lang scheme
(define merge
  (lambda (s r)
    (cond [(empty? s) r]
          [(empty? r) s]
          [else 
           (if (<= (car s) (car r))
               (cons (car s) (merge (cdr s) r))
               (cons (car r) (merge s (cdr r))))])))

(define part
  (lambda (s a b)
    (if (> a 1) (part (rest s) (sub1 a) (sub1 b))
        (if (= a b)
            (list (car s))
            (cons (car s) (part (cdr s) a (sub1 b)))))))

(define s '(1 3 5))
(define r '(2 4 6))

;(merge s r)
(part '(1 2 3) 2 3)