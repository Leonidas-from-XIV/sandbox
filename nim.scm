#lang scheme
;;;; An implementation of the Nim game
;;;; 2009 by Marek Kubica <marek@xivilization.net>
;;;; This code is free under the MIT License
;;;;
;;;; For the rules check <http://en.wikipedia.org/wiki/Nim>

(define make-field
  (lambda (rows maximum-in-row)
    (if (= rows 1) (list (add1 (random maximum-in-row)))
        (cons (add1 (random maximum-in-row))
              (make-field (sub1 rows) maximum-in-row)))))

(define display-field
  (lambda (field)
    (define (display-line value)
      (if (= value 0) (display "\n")
          (begin
            (display "|")
            (display-line (sub1 value)))))
    ;; actual code
    (if (empty? (cdr field)) 
          (display-line (car field))
        (begin
          (display-line (car field))
          (display-field (cdr field))))))

(define take-from-row
  (lambda (field row howmany)
    ;; to filter out empty rows, we don't need them
    (define (filter-empty lat)
      (filter (lambda (num) (> num 0)) lat))
    
    (filter-empty
     ;; subtract the given value from the given index
     (if (= row 0) (cons (- (car field) howmany) (cdr field))
         (cons (car field) 
               (take-from-row (cdr field) (sub1 row) howmany))))))