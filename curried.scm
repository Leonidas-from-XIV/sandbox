#lang scheme
(require htdp/testing)

;;; a manually curried division function
(define manually-curried
  (lambda (a)
    (lambda (b)
      (/ a b))))

;;; three layer deep currying
(define three-layers
  (lambda (first)
    (lambda (second)
      (lambda (third)
        (+ first second third)))))

;;; call/curry macro that calls functions recursively
(define-syntax call/curry
  (syntax-rules ()
    [(_ fun single-arg) (fun single-arg)]
    [(_ fun first-arg ... last-arg) ((call/curry fun first-arg ...) last-arg)]))

;;; test results
(check-expect ((manually-curried 1) 2) (/ 1 2))
(check-expect (call/curry manually-curried 1 2) (/ 1 2))
(check-expect (((three-layers 1) 2) 3) 6)
(check-expect (call/curry three-layers 1 2 3) 6)

;;; generate a report
(generate-report)