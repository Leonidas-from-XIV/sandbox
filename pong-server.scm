#lang scheme
(define pong-server
  (thread
   (lambda ()
     (let loop ()
       (let [(msg (thread-receive))]
         (if (and (list? msg)
                  (= (length msg) 2)
                  (eq? (cadr msg) 'ping))
             (let [(from (car msg))]
               (thread-send from 'pong)
               (loop))
             (loop)))))))

(thread-send pong-server (list (current-thread) 'ping))
(thread-receive)