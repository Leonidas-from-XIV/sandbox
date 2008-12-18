#lang scheme
(require srfi/13)

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