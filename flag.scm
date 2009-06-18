#lang scheme
(define dimensions '(50 80))

(define display-row
  (lambda (character length)
    (if (= length 0) (display "\n")
        (begin
          (display character)
          (display-row character (sub1 length))))))

(define list/chars
  (lambda (height)
    (build-list height
                (lambda (number)
                   (if (>= number (/ height 2)) "0"
                       "1")))))

(define display-flag
  (lambda (width height)
    (void 
     (map (curryr display-row width) (list/chars height)))))

(apply display-flag dimensions)