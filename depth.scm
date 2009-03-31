#lang scheme
;;;; List depth computation
;;;; 2009 by Marek Kubica <marek@xivilization.net>
;;;; This code is free under the MIT License
(require htdp/testing)

;;; the depth is how many lists are inside a list
;;; only covers lists and symbols
(define depth
  (lambda (lat)
    (cond [(empty? lat) 0]
          [(symbol? (car lat)) (depth (cdr lat))]
          [else (add1 (max (depth (car lat))
                           ;; need to subtract 1 because cdr wraps the result in a list
                           (sub1 (depth (cdr lat)))))])))

;; check with empty list
(check-expect (depth '()) 0)
;; only symbols
(check-expect (depth '(a b c)) 0)
;; two lists
(check-expect (depth '((a) (b))) 1)
;; symbols and list
(check-expect (depth '(a (b c) d)) 1)
;; more deeply nested lists
(check-expect (depth '(q (s v) w ((p) t) r)) 2)
;; show results
(generate-report)