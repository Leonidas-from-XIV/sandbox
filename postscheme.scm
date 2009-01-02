#lang scheme
;;;; A try on macros by defining a useless DSL that changes Scheme code from
;;;; a prefix language to a postfix language. This is all half-jokingly
;;;; but useful for learning how to use macros.
;;;;
;;;; 2009 by Marek Kubica

;;; the postfixing macro, that apply the transformation on itself and
;;; every operand.
(define-syntax postfixed
  (syntax-rules ()
    ((_ (operands ... operator)) (operator (postfixed operands) ...))
    ((_ atom) atom)))

;; all of these return 5
(postfixed 5)
(postfixed (2 3 +))
(postfixed (2 (1 2 +) +))
(postfixed ((1 1 +) (1 2 +) +))