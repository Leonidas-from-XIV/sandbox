#lang scheme
;;;; Implementation of Red Black Trees

;;; define a type
(define-struct rb-node
  (left right value parent color)
  #:transparent #:mutable)

(define left-rotate
  (lambda (T x)
    (let ([y (rb-node-right x)])
      (begin
        (set-rb-node-right! x (rb-node-left y))
        (if (not (eq? (rb-node-left y) (void)))
            (set-rb-node-parent! (rb-node-left y) x)
            #f)
        (set-rb-node-parent! y (rb-node-parent x))
        (cond 
          [(eq? (rb-node-parent x) (void))
           ;; what is root[T]?
           (#f)]
          [(eq? x (rb-node-left (rb-node-parent x)))
           (set-rb-node-left! (rb-node-parent x) y)]
          [else (set-rb-node-right! (rb-node-parent x) y)])
        (set-rb-node-left! y x)
        (set-rb-node-parent! x y)))))

(define sample
  (make-rb-node (void) (void) 3 (void) 'black))