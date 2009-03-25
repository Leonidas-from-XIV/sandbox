#lang scheme
;;;; Haskell-like function call semantics for Scheme
;;;; 2009 by Marek Kubica <marek@xivilization.net>
;;;; This code is free under the MIT License
;;;;
;;;; This file defines a new macro, lambda/curry that can be used to create
;;;; automatically functions that are curried automatically just by calling
;;;; them in an ordinary function call.
;;;; Another macro, call/lambda is thrown in for some syntactic sugar.

;;; use the HtDP testing tools to check expectations
(require htdp/testing)

;;; call/curry macro that calls functions recursively. This is just 
;;; syntactic sugar for function calls like ((curried-add 1) 2) which
;;; become (call/curry curried-add 1 2)
(define-syntax call/curry
  (syntax-rules ()
    [(_ fun single-arg) (fun single-arg)]
    [(_ fun first-arg ... last-arg) ((call/curry fun first-arg ...) last-arg)]))

;;; creates curried functions by taking the first argument of the function
;;; creating a lambda with one argument, inside that another lambda with
;;; the second argument as single argument, and so on, unless all arguments
;;; are used once. The body is left unchanged
(define-syntax lambda/curry
  (syntax-rules ()
    ;; 0 and 1 arguments, simple lambda
    [(_ () body) (lambda () body)]
    [(_ (single-arg) body) (lambda (single-arg) body)]
    ;; 2 or more arguments, split arguments and build recursive lambdas
    [(_ (first-arg second-arg ...) body)
     (lambda (first-arg)
       (lambda/curry (second-arg ...) body))]))

;;; Simple, manually curried functions
;;; a division function with two arguments
(define manually-curried
  (lambda (a)
    (lambda (b)
      (/ a b))))

;;; three layer deep currying = three arguments
(define three-layers
  (lambda (first)
    (lambda (second)
      (lambda (third)
        (+ first second third)))))

;;; test results
(check-expect ((manually-curried 1) 2) (/ 1 2))
(check-expect (call/curry manually-curried 1 2) (/ 1 2))
(check-expect (((three-layers 1) 2) 3) 6)
(check-expect (call/curry three-layers 1 2 3) 6)
;; test definition of 0 argument function
(check-expect ((lambda/curry () 'something)) 'something)
(check-expect ((lambda/curry (val) val) 'something) 'something)
(check-expect (((lambda/curry (first second) (+ first second)) 1) 2) 3)
(check-expect (call/curry (lambda/curry (first second) (+ first second)) 1 2) 3)

;;; generate a report
(generate-report)