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

(define (fak number)
  (if (= number 0)
         1
      (* (fak (- number 1)) number)))
