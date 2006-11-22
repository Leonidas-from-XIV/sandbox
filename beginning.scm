; A small file with Scheme code
; Things that can be learned from this:
; - Scheme files usually have the extension .scm
; - Scheme comments are started with semicolon
; - files can be loaded using (load "filename.scm")

(define pi 3.14159)

(define radius 10)

(* pi (* radius radius))

(define umfang (* 2 pi radius))

(define (quadrat x) (* x x))

; a recursive way to net the n-th power of a number
(define (pow number power)
  (if (= power 2) (* number number)
      (* (pow number (- power 1)) number)))
