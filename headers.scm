#lang scheme
  
(require web-server/servlet)
(require web-server/servlet-env)
(provide/contract (start (request? . -> . response?)))

(define (display-header header)
  `(li (strong ,(bytes->string/utf-8 (header-field header) )) 
       ": "
       ,(bytes->string/utf-8 (header-value header))))

(define (start request)
  `(html
    (head (title "Request Headers"))
    (body (ul ,@(map display-header (request-headers/raw request))))))

(serve/servlet start
               #:launch-browser? #f
               #:port 4113)