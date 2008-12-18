#lang scheme
(require srfi/13)
(require srfi/41)

;; port->stream married with string-tokenize results in port->stream-of-tokens
(define (port->stream-of-tokens . port)
  (define port->stream-of-tokens
    (stream-lambda (p)
                   (let [(line (read-line p))]
                     (if (eof-object? line)
                         stream-null
                         (stream-append
                          (list->stream (string-tokenize line)) 
                          (port->stream-of-tokens p))))))
  (let [(p (if (null? port) (current-input-port) (car port)))]
    (if (not (input-port? p))
        (error 'port->stream "non-input-port argument")
        (port->stream-of-tokens p))))

; returns a hashmap mapping words to their frequency
(define frequencies
  (foldl (lambda (word hash)
         (let [(current-value (hash-ref hash word 0))]
           (hash-set hash word (add1 current-value))))
       (make-immutable-hash '())
       ; feed in the separated words
       (string-tokenize (port->string (current-input-port)))))

(define freq-list (hash-map frequencies
                            (lambda (key value)
                              (list value key))))

(define sorted-freq (sort freq-list
                          (lambda (first second)
                            (> (car first) (car second)))))

(for-each (lambda (item)
            (printf "~a ~a~n" (car item) (cadr item))) sorted-freq)