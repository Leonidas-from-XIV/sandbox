#lang web-server/insta

(define (display-header header)
  `(li (strong ,(bytes->string/utf-8 (header-field header) )) 
       ": "
       ,(bytes->string/utf-8 (header-value header))))

(define (start request)
  `(html
    (head (title "Request Headers"))
    (body (ul ,@(map display-header (request-headers/raw request))))))