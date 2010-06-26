(import (rnrs)
        (rnrs io ports)
        (ypsilon socket))

(define (connection-handler socket)
    (let* ((binary-port (socket-port socket))
           (port (transcoded-port binary-port (make-transcoder (utf-8-codec) 'crlf))))
      (hear port)
      (say port "HELLO")
      (hear port)
      (hear port)
      (let* ((numbers (map (lambda (dummy)
                             (string->number (hear port)))
                           '(1 2 3)))
             (sum (fold-left + 0 numbers))
             (string-sum (number->string sum)))
        (say port string-sum)
        (hear port))))

(define (hear port)
  (let ((read (get-line port)))
    (print read)
    read))

(define (say port string)
  (print (string-append "--> " string))
  (put-string port (string-append string "\n")))

(define (print msg)
  (display (string-append msg "\n")))

(call-with-socket
 (make-client-socket "ilab.net.in.tum.de" "2342")
 connection-handler)