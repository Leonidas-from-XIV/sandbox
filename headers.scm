#lang web-server/insta
(require web-server/private/request-structs)

(define (display-header header)
  `(li ,(bytes->string/utf-8 (header-field header))))

;(define (display-header header)
;  "Header")

(define (start request)
  (display (map display-header (request-headers/raw request)))
  `(html
    (head (title "Request Headers"))
    (body (ul ,@(map display-header (request-headers/raw request))))))
    ;(body (ul (li "abc") (li "def")))))