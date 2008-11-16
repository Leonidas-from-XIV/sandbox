#lang scheme
;;; A mergesort implemented in Scheme based on the slides
;;; of Prof. Broy

;; merges two already-sorted lists together so the output
;; is another sorted list
;; (Contrary to the slides we use <= and not >= here)
(define merge
  (lambda (s r)
    (cond [(empty? s) r]
          [(empty? r) s]
          [else 
           (if (<= (car s) (car r))
               (cons (car s) (merge (cdr s) r))
               (cons (car r) (merge s (cdr r))))])))

;; cuts out elements from a list, namely from index a to b
;; (watch out: it begins with 1 and not zero)
(define part
  (lambda (s a b)
    (if (> a 1) (part (rest s) (sub1 a) (sub1 b))
        (if (= a b)
            (list (car s))
            (cons (car s) (part (cdr s) a (sub1 b)))))))

;; the actual mergesort
(define mergesort
  (lambda (s)
    (if (<= (length s) 1) s
        (let* ([h (quotient (length s) 2)]
               [lpart (part s 1 h)]
               [rpart (part s (add1 h) (length s))])
          (merge (mergesort lpart) (mergesort rpart))))))

;; some testing
;(define s '(1 3 5))
;(define r '(2 4 6))
;(merge s r)
;(part '(1 2 3) 2 3)
(mergesort '(3 5 2))