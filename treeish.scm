#lang scheme
;;;; Implementations of tree operations from the Info1 slides by Prof. Broy
;;;; Use them at your own risk

(define etree '())

(define etree? empty?)

(define tree-cons
  (lambda (left root right)
    (list left root right)))

(define left
  (lambda (tree)
    (if (= (length tree) 3) (car tree)
        etree)))

(define right
  (lambda (tree)
    (if (= (length tree) 3) (caddr tree)
        etree)))

(define root cadr)

(define height
  (lambda (tree)
    (if (etree? tree) 0
        (+ 1 (max (height (left tree)) (height (right tree)))))))

(define pre-order
  (lambda (tree)
    (if (etree? tree) '()
        (append
         (list (root tree))
         (pre-order (left tree))
         (pre-order (right tree))))))

(define in-order
  (lambda (tree)
    (if (etree? tree) '()
        (append 
         (in-order (left tree))
         (list (root tree))
         (in-order (right tree))))))

(define post-order
  (lambda (tree)
    (if (etree? tree) '()
        (append 
         (post-order (left tree))
         (post-order (right tree))
         (list (root tree))))))

(define simple (tree-cons (tree-cons etree 1 etree) 2 etree))