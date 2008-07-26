; A try on macros by defining a useless DSL
; 2008 by Marek Kubica

; transform list (a b c d) to (b c d a)
(define push-contents
  (lambda (lst)
    (append (cdr lst) (list (car lst)))))

; transform list (b c d a) to (a b c d)
(define pop-contents
  (lambda (lst)
    (let ((reversed (reverse lst)))
      (cons (car reversed) (reverse (cdr reversed))))))
