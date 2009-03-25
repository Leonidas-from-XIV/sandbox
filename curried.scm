#lang scheme

(define manually-curried
  (lambda (a)
    (lambda (b)
      (/ a b))))

((manually-curried 1) 2)

(define three-layers
  (lambda (first)
    (lambda (second)
      (lambda (third)
        (+ first second third)))))

(((three-layers 1) 2) 3)

(define-syntax call/curry
  (syntax-rules ()
    [(_ fun first-arg ... last-arg) ((fun first-arg) ... last-arg)]))

(call/curry manually-curried 1 2)
(call/curry three-layers 1 2 3)