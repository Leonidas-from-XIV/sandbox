#lang scheme
;;;; Simple garden fence encryption algorithm
;;;; 2009 by Marek Kubica <marek@xivilization.net>
;;;; This code is free under the MIT License
;;;;
;;;; How does it work:
;;;; Provided that the cleartext is "diesisteinklartext" then
;;;; the text has to be processed into a zigzag-like form.
;;;; The "key" is the number of lines in the zigzag.
;;;;
;;;; An example:
;;;;
;;;; 1. d         k         = dk
;;;; 2.  i       n l        = inl
;;;; 3.   e     i   a       = eia
;;;; 4.    s   e     r   t  = sert
;;;; 5.     i t       t x   = ittx
;;;; 6.      s         e    = se
;;;;
;;;; Then the resulting encrypted text is "dkinleiasertittxse",
;;;; which results in appending the characters from all lines, starting
;;;; with the first.
;;;;
;;;; More details (in german) on the web:
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
  (lambda (chars move times)
    (if (= times 1) (list (every-nth-item chars move))
        (cons (every-nth-item chars move)
              (build-phases (cdr chars) move (- times 1))))))

;;; encrypts a string using the given height to a new, encrypted string
(define encrypt
  (lambda (text height)
    (let* ([chars (string->list text)]
           ;; how many chars to skip in one phase
           [move (- (* height 2) 2)]
           [times move]
           [up/down-phases (build-phases chars times move)]
           [unified-phases (merge-phases up/down-phases height)])
      (apply string-append (map list->string unified-phases)))))

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
    
    ;; merge phases together as long as begin < end
    (if (>= begin end) '()
        (cons (merge-two-phases lat begin end)
              (merge-up/down-phases lat (+ begin 1) (- end 1))))))

;;; Interweaves two lists into one, like a flattened version of zip
;;; (a b c) (d e f) -> (a d b e c f)
(define interweave
  (lambda (head-lat tail-lat)
    (cond [(null? head-lat) tail-lat]
          [(null? tail-lat) head-lat]
          [else ;; (first-of-head first-of-tail recursive-rest)
           `(,(car head-lat) ,(car tail-lat) 
                             ,@(interweave (cdr head-lat) (cdr tail-lat)))])))

;;; generates the "gartenzaun" for the given height and text but instead
;;; of letters, it uses numbers, so the result can be used to map
;;; letters back to their original position in the cleartext
(define generate-linecodes
  (lambda (text height)
    (let* ([len (string-length text)]
           [move (- (* height 2) 2)]
           [times move])
      (merge-phases 
       (build-phases (build-list len values) move times)
       height))))

;;; flattens the results into a single list, so it can be converted
;;; into a string
(define flatten-linecode
  (lambda (code)
    (if (null? code) '()
        (append (car code) (flatten-linecode (cdr code))))))

;;; decrypts an encrypted text using the given height as key
;;; works by constructing a zigzag structure but not containing the
;;; letters but rather the positions in the original string
;;; the positions get mapped to letters to reconstruct the plaintext
(define decrypt
  (lambda (text height)
    (let ([cleartext
           (foldl (lambda (item hash) (hash-set hash (car item) (cadr item)))
                  (make-immutable-hash '())
                  (zip (flatten-linecode (generate-linecodes text height))
                       (string->list text)))])
      ;; return the values in the hash in sorted order and construct a string
      (list->string (map
                     (lambda (index) (hash-ref cleartext index)) 
                     (build-list (string-length text) values))))))

;; some demonstration code
(encrypt "diesisteinklartext" 6)
(decrypt (encrypt "diesisteinklartext" 6) 6)