#lang scheme
;;; pseudo RSA stuff for the GBS sheet 13 (WS0910)
;;; purely functional solution without loops, mutation, IO
;;; (c) 2010 by Marek Kubica

;; some definitions from the sheet
(define p 47)
(define q 71)
(define n (* p q))
(define x (* (- p 1) (- q 1)))
(define e 79)
(define d 1019)
(define message 688232687966668003)

;; gets the magnitude of a number: 1000 = 10e3
;; magnitude returns the 3
(define magnitude
  (lambda (number)
    (if (<= number 9) 0
        (add1 (magnitude (quotient number 10))))))

;; exponentiation function
(define exp (curry expt 10))

;; split number in smaller numbers of some specific length
(define chunk
  (lambda (number chunk-size)
    (let*-values ([(exp-difference) (sub1 chunk-size)]
                  [(q r)
                   (quotient/remainder number (exp (- (magnitude number) exp-difference)))])
      (if (<= (magnitude r) exp-difference) (list q r)
          (cons q (chunk r chunk-size))))))

;; split the number into the biggest chunks that are smaller than n
(define smaller-chunks
  (lambda (message n)
    (let* ([n-size (magnitude n)]
           [candidates (chunk message (add1 n-size))])
      ; if the candidate list contains some numbers larger than n, create a
      ; new chunked list with smaller numbers
      (if (andmap (lambda (element) (< element n)) candidates) candidates
          (chunk message n-size)))))

;; get the maximal magnitude of a list of numbers
(define maximal-magnitude
  (lambda (numbers)
    (apply max (map magnitude numbers))))

;; constructs a number by repeatedly multiplicating
;; '(12 3 4) -> 120304
(define construct-message
  (lambda (numbers)
    (let ([shift-with (exp (add1 (maximal-magnitude numbers)))])
      (foldl (lambda (new msg) (+ (* msg shift-with) new)) 0 numbers))))

;; do the parsing (splitting), exponentiation and modulo
(define encrypt
  (lambda (message e n)
    (map (lambda (element) (remainder (expt element e) n))
         (smaller-chunks message n))))

;; inversion of encrypt
(define decrypt
  (lambda (blocks d n)
    (construct-message
     (map (lambda (element) (remainder (expt element d) n))
          blocks))))

;; roundtrip works
(decrypt (encrypt message e n) d n)