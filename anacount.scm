(define frequency (make-hash-table 'equal))

; provided for compatibility to Mz 209, Mz 360 does not need this cruft
(define (zero)
  0
  )

; please, only feed characters here, not strings
(define (analyze-frequency element)
  (hash-table-put! 
    frequency element (+ (hash-table-get frequency element zero) 1)
    )
  )

(define (calculate-frequency str)
  (map analyze-frequency (string->list str)
       )
  )

;(hash-table-map frequency (lambda (key value) value))

(define (sum-worker lst s)
   (if (null? lst)
       s
       (sum-worker (cdr lst) (+ s (car lst)))))

(define (sum lst)
  (sum-worker lst 0))

(define (fak number)
  (if (= number 0)
         1
      (* (fak (- number 1)) number)))

(define (get-divisor hash)
  (let ((divisor (sum (hash-table-map frequency (lambda (key value) value)))))
    (if (= divisor 0)
        1
	((lambda (n) n) divisor)
      )))

