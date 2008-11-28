#lang scheme
(define fib-seq
  (lambda (n)
    (cond [(= n 0) '(0)]
          [(= n 1) '(1 0)]
          [else (let* ((seq (fib-seq (sub1 n)))
                      (a (car seq))
                      (b (cadr seq)))
                  (cons (+ a b) seq))])))