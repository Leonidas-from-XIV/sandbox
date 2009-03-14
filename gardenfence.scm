#lang scheme
;;;; Simple garden fence encryption algorithm
;;;; <http://www.webplain.de/foren/read.php?1,8094>

;;; for DROP
(require srfi/1)

;;; returns every nth item of a list, starting with the first
(define every-nth-item
  (lambda (lat n)
    (cond [(empty? lat) '()]
          [(> n (length lat)) (list (car lat))]
          [else (cons (car lat)
                      (every-nth-item (drop lat n) n))])))

;;; creates a list of numbers of all phases
(define build-phases
  (lambda (chars times move)
    (cond [(= times 1) (list (every-nth-item chars move))]
          [else (cons (every-nth-item chars move)
                      (build-phases (cdr chars) (- times 1) move))])))

(define encode-clean
  (lambda (text height)
    (let* ([chars (string->list text)]
          ;; how many chars to skip in one phase
          [move (- (* height 2) 2)]
          [times move])
      (build-phases chars times move))))

;;; merges up and down phases and leaves the top and bottom phases alone
(define merge-phases
  (lambda (lat height)
    (let* ([len (length lat)]
           [top (car lat)]
           ;; the last element
           [bottom (list-ref lat (- height 1))]
           ;; merge all items except for the first and last
           [grouped (merge-up/down-phases lat 1 (- len 1))]
           [merged (map (lambda (arg) (apply interweave arg)) grouped)])
      ;; connect top, the merged phases and bottom phases together
      `(,top ,@merged ,bottom))))

;;; helper for merge-phases, does the actual merging
(define merge-up/down-phases
  (lambda (lat begin end)
    (define merge-two-phases
      (lambda (lat first second)
        (let ([head (list-ref lat first)]
              [tail (list-ref lat second)])
          (list head tail))))
    
    (if (>= begin end) '()
        (cons (merge-two-phases lat begin end)
              (merge-up/down-phases lat (+ begin 1) (- end 1))))))

;;; (a b c) (d e f) -> (a d b e c f)
(define interweave
  (lambda (head-lat tail-lat)
    (cond [(null? head-lat) tail-lat]
          [(null? tail-lat) head-lat]
          [else ;; (first-of-head first-of-tail recursive-rest)
           `(,(car head-lat) ,(car tail-lat) 
                             ,@(interweave (cdr head-lat) (cdr tail-lat)))])))

;;; flattens a list of lists of chars into a string
(define flatten
  (lambda (lat)
    (apply string-append (map list->string lat))))

(define a (encode-clean "diesisteinklartext" 6))
(define b (merge-phases a 6))
b
;(define c (flatten b))

;;; like enumerate in Python
(define enumerate
  (lambda (lat)
    (let ([indices (build-list (length lat) values)])
      (zip indices lat))))

;;; like enumerate but cycles the argument
(define enumerate-cycle
  (lambda (lat size)
    ;; calculate the modulo size of all indizes
    ;; curry creates a partial function
    (let ([indices (map (curryr modulo size) (build-list (length lat) values))])
      (zip indices lat))))

(define generate-linecodes
  (lambda (text height)
    (let ([len (string-length text)])
      (merge-phases (build-phases (build-list len values) 10 10)))))

(define flatten-linecode
  (lambda (code)
    (if (null? code) '()
        (append (car code) (flatten-linecode (cdr code))))))

;(generate-linecodes c 6)
;(flatten-linecode (generate-linecodes c 6))

(define decrypt
  (lambda (text height)
    (let ([cleartext (foldl (lambda (item hash) (hash-set hash (car item) (cadr item)))
                            (make-immutable-hash '())
                            (zip (flatten-linecode (generate-linecodes text height))
                                 (string->list text)))])
      (list->string (map 
                     (lambda (index) (hash-ref cleartext index)) 
                     (build-list (string-length text) values))))))

;(decrypt c 6)