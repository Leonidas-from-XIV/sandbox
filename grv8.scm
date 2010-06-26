(import (rnrs)
        (rnrs io ports)
        (ypsilon socket))

(define connection-handler
  (lambda (socket)
    (let* ((binary-port (socket-port socket))
           (port (transcoded-port binary-port (make-transcoder (utf-8-codec) 'crlf))))
      (print (get-line port))
      (say port "HELLO")
      (print (get-line port))
      (print (get-line port))
      (let* ((numbers (map (lambda (dummy)
                             (string->number (get-line port)))
                           '(1 2 3)))
             (sum (fold-left + 0 numbers)))
        (say port (number->string sum))
        (print (get-line port))))))

(define say
  (lambda (port string)
    (print (string-append "--> " string))
    (put-string port (string-append string "\r\n"))))

(define (print msg)
  (display (string-append msg "\n")))

(define main
  (lambda ()
    (let ((socket (make-client-socket "ilab.net.in.tum.de" "2342")))
      (call-with-socket socket connection-handler))))

(main)