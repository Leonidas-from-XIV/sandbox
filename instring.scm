#lang scheme
; instead of every and any PLT also supports andmap and ormap respectively
; thus making SRFI 1 unneccessary
(require srfi/1)
; alternatively there is SFRI 13 string-every which saves a let-binding,
; but does not make things considerably easier

(define chars-in-string?
  (lambda (chars string)
    (let ([char-list (string->list chars)]
          [string-list (string->list string)])
      (every (lambda (char)
               (any (lambda (string-char)
                      (eq? char string-char)) string-list))
               char-list))))

(chars-in-string? "bc" "bcd")