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

(define-syntax lambda/curry
  (syntax-rules ()
    [(_ () body) (lambda () body)]
    [(_ (single) body) (lambda (single) body)]))

(define just-false
  (lambda/curry ()
              #f))

(just-false)

(define pass-through
  (lambda/curry (val)
                val))

(pass-through 1)

;;; test results
(check-expect ((manually-curried 1) 2) (/ 1 2))
(check-expect (call/curry manually-curried 1 2) (/ 1 2))
(check-expect (((three-layers 1) 2) 3) 6)
(check-expect (call/curry three-layers 1 2 3) 6)

;;; generate a report
(generate-report)