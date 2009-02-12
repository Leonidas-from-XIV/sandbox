#lang scheme
;;;; Implementations of tree operations from the Info1 slides by Prof. Broy
;;;; The implementations are adapted mainly from slide set 13
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

;;; slide 18 example 1, complete binary tree
(define example1 
  (tree-cons 
   (tree-cons etree 1 etree)
   2
   (tree-cons etree 3 etree)))

;;; example 2, non-complete binary tree
(define example2
  (tree-cons
   etree
   1
   (tree-cons
    etree
    2
    (tree-cons etree 3 etree))))

;;; slide 23
(define complete?
  (lambda (tree)
    (if (etree? tree) #t
        (let [(l (left tree))
              (r (right tree))]
          ;; the actual condition
          (and
           (= (height l) (height r))
           (complete? l)
           (complete? r))))))

;;; generator of fancy binary trees
(define h
  (lambda (n i)
    (if (= n 0) etree
        (tree-cons 
         (h (- n 1) (* i 2))
         i
         (h (- n 1) (+ (* i 2) 1))))))

;;; simplify call of h
(define gen
  (lambda (n)
    (h n 1)))

;;; checks whether the tree is a choice tree
(define choice-tree?
  (lambda (tree)
    (cond
      [(etree? tree) #t]
      [(etree? (left tree)) (etree? (right tree))]
      [(etree? (right tree)) (etree? (left tree))]
      [else
       (and
        (= (root tree) (max (root (left tree)) (root (right tree))))
        (choice-tree? (left tree))
        (choice-tree? (right tree)))])))

(define make-choice-tree
  (lambda (seq)
    (cond 
      [(empty? seq) etree]
      [(= (length seq) 1) (tree-cons etree (car seq) etree)]
      [else 
       (let [(h (quotient (length seq) 2))]
         (concat-choice-tree
          (make-choice-tree (part seq 1 h))
          (make-choice-tree (part seq (+ h 1) (length seq)))))])))

(define part
  (lambda (seq a b)
    (cond
      [(> a 1) (part (cdr seq) (- a 1) (- b 1))]
      [(= a b) (list (car seq))]
      [else
       (append
        (list (car seq))
        (part (cdr seq) a (- b 1)))])))

(define concat-choice-tree
  (lambda (l r)
    (cond
      [(etree? r) l]
      [(etree? l) r]
      [(> (root r) (root l)) (tree-cons l (root r) r)]
      [else (tree-cons l (root l) r)])))