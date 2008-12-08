#lang scheme

(define in (port->string (current-input-port)))

(define frequencies
  (foldl (lambda (word hash)
         (let [(current-value (hash-ref hash word 0))]
           (hash-set hash word (add1 current-value))))
       (make-immutable-hash '())
       (regexp-split #px"\\s" in)))

(define freq-list (hash-map frequencies
                            (lambda (key value)
                              (list value key))))

(define sorted-freq (reverse (sort freq-list
                                   (lambda (first second)
                                     (< (car first) (car second))))))

(for-each (lambda (item)
            (printf "~a ~a~n" (car item) (cadr item))) sorted-freq)