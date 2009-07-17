#lang scheme
;;;; Implementation of Red Black Trees

;;; define a type
(define-struct rb-node
  (left right value parent color)
  #:transparent #:mutable)

(define set-rb-node-root!
  (lambda (node new-root)
    ;; if it is the parent node: bad luck, can't change
    (cond [(eq? (rb-node-parent node) (void)) (void)]
          ;; if it is the direct child of the root: change the root
          [(eq? (rb-node-parent (rb-node-parent node)) (void))
           (set-rb-node-parent! node new-root)]
          ;; if it is neither: go upwards until one of the cases fits
          [else (set-rb-node-root! (rb-node-parent node) new-root)])))

(define left-rotate
  (lambda (T x)
    (let ([y (rb-node-right x)])
      (begin
        (set-rb-node-right! x (rb-node-left y))
        (if (not (eq? (rb-node-left y) (void)))
            (set-rb-node-parent! (rb-node-left y) x)
            #f)
        (set-rb-node-parent! y (rb-node-parent x))
        (if (eq? (rb-node-parent x) (void))
            ;; root[T] <- y
            (set-rb-node-root! T y)
            ;; else
            (if (eq? x (rb-node-left (rb-node-parent x)))
                (set-rb-node-left! (rb-node-parent x) y)
                (set-rb-node-right! (rb-node-parent x) y)))
        (set-rb-node-left! y x)
        (set-rb-node-parent! x y)))))

(define sample
  (make-rb-node (void) (void) 3 (void) 'black))