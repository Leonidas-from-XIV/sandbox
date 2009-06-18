#lang scheme
(define-struct rb-node
  (left right value parent color)
  #:transparent)

(make-rb-node (void) (void) 3 (void) 'black)