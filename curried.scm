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
    [(_ fun single-arg) (fun single-arg)]
    [(_ fun first-arg ... last-arg) ((call/curry fun first-arg ...) last-arg)]))

(call/curry manually-curried 1 2)
(call/curry three-layers 1 2 3)