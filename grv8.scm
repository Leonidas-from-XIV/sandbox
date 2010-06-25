(import (rnrs)
        (rnrs io ports)
	(ypsilon socket))

(define connection-handler
  (lambda (socket)
    (let* ((binary-port (socket-port socket))
           (port (transcoded-port binary-port (make-transcoder (utf-8-codec) 'crlf))))
      (display (read port)))))

(define main
  (lambda ()
    (let ((socket (make-client-socket "ilab.net.in.tum.de" "2342")))
      (call-with-socket socket connection-handler))))

(main)