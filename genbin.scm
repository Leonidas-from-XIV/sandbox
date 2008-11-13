#lang scheme
(define genbin
  (lambda (n)
    (if (= n 0) '(())
        (append
         (appall (genbin (sub1 n)) #t)
         (appall (genbin (sub1 n)) #f)))))

(define appall
  (lambda (s b)
    (if (= (length s) 1) (list (cons b (car s)))
        (list
         (cons b (car s)) 
         (car (appall (cdr s) b))))))

(genbin 2)