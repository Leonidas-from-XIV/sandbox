; A try on macros by defining a useless DSL
; 2008 by Marek Kubica
; <http://ircbrowse.com/channel/scheme/20080726>

; transform list (a b c d) to (b c d a)
(define push-contents
  (lambda (lst)
    (append (cdr lst) (list (car lst)))))

; transform list (b c d a) to (a b c d)
(define pop-contents
  (lambda (lst)
    (let ((reversed (reverse lst)))
      (cons (car reversed) (reverse (cdr reversed))))))

; sjamaan & elfs solution to the problem
; Yeah, it's nice and makes my functions useless
; at least I can try to get the macro to apply recursively
(define-syntax postfixed 
  (syntax-rules () 
    ((_ (?operands ... ?operator)) (?operator ?operands ...))))

; works as expected
(postfixed (2 3 +))
; works, but should be (1 2 +) -> apply macro recursively
(postfixed (2 (+ 1 2) +))