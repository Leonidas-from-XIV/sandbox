#lang scheme
;;;; Implementation of red black trees
;;;; 2009 by Marek Kubica <marek@xivilization.net>
;;;; This code is free under the MIT License
;;;;
;;;; The implementation is taken from CLRS 2nd Edition
;;;; and adapted for PLT Scheme 4.

;;; define the representation of one single node
(define-struct rb-node
  ((left #:mutable)
   (right #:mutable)
   value
   (parent #:mutable)
   (color #:mutable)))

;;; define the representation of the whole tree
(define-struct rb-tree
  (root) #:mutable #:transparent)

(define generate-rotate
  (lambda (node set-node! contra-node set-contra-node!)
    (lambda (T x)
      (let ([y (contra-node x)])
        (set-contra-node! x (node y))
        (if (not (eq? (node y) (void)))
          (set-rb-node-parent! (node y) x) #f)
        (set-rb-node-parent! y (rb-node-parent x))
        (if (eq? (rb-node-parent x) (void))
            ;; root[T] <- y
            (set-rb-tree-root! T y)
            ;; else
            (if (eq? x (node (rb-node-parent x)))
                (set-node! (rb-node-parent x) y)
                (set-contra-node! (rb-node-parent x) y)))
        (set-node! y x)
        (set-rb-node-parent! x y)))))

(define left-rotate
  (generate-rotate rb-node-left set-rb-node-left! rb-node-right set-rb-node-right!))

(define right-rotate
  (generate-rotate rb-node-right set-rb-node-right! rb-node-left set-rb-node-left!))

(define rb-insert
  (lambda (T z)
    (let* ([y (void)]
           [x (rb-tree-root T)])
      #f)))

;;; sample code for trying stuff out

(define x
  (make-rb-node (void) (void) 'x (void) 'black))

(define y
  (make-rb-node (void) (void) 'y (void) 'black))

(define alpha
  (make-rb-node (void) (void) 'alpha (void) 'black))

(define beta
  (make-rb-node (void) (void) 'beta (void) 'black))

(define gamma
  (make-rb-node (void) (void) 'gamma (void) 'black))

;; attach alpha to x
(set-rb-node-left! x alpha)
(set-rb-node-parent! alpha x)
;; attach y to x
(set-rb-node-right! x y)
(set-rb-node-parent! y x)
;; attach beta to y
(set-rb-node-left! y beta)
(set-rb-node-parent! beta y)
;; attach gamma to y
(set-rb-node-right! y gamma)
(set-rb-node-parent! gamma y)
;; create a tree with root x
(define T
  (make-rb-tree x))

;; check what we had
(rb-node-value (rb-tree-root T))
;; rotate
(left-rotate T x)
;; check what we got
(rb-node-value (rb-tree-root T))
;; rotate back
(right-rotate T y)
;; re-check
(rb-node-value (rb-tree-root T))

(rb-insert T (make-rb-node (void) (void) 3 (void) 'black))