#lang scheme
(require srfi/1)

(define chars-in-string?
  (lambda (chars string)
    (let ([char-list (string->list chars)]
          [string-list (string->list string)])
      (every (lambda (char)
               (any (lambda (string-char)
                      (eq? char string-char)) string-list))
               char-list))))

(chars-in-string? "bc" "bcd")