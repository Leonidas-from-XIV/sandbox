#lang scheme
;;;; Implementation of red black trees
;;;; 2009 by Marek Kubica <marek@xivilization.net>
;;;; This code is free under the MIT License
;;;;
;;;; The implementation is taken from CLRS 2nd Edition and adapted for 
;;;; PLT Scheme 4.
;;;;
;;;; A good applet demonstrating the algorithm as in CLRS can be found at
;;;; <http://people.ksp.sk/~kuko/bak/index.html>

;;; define the representation of one single node
(define-struct rb-node
  ((left #:mutable)
   (right #:mutable)
   value
   (parent #:mutable)
   (color #:mutable)))

;;; define the representation of the whole tree
(define-struct rb-tree
  (root) #:mutable #:transparent)

(define generate-rotate
  (lambda (node set-node! contra-node set-contra-node!)
    (lambda (T x)
      (let ([y (contra-node x)])
        (set-contra-node! x (node y))
        (if (not (eq? (node y) (void)))
          (set-rb-node-parent! (node y) x) #f)
        (set-rb-node-parent! y (rb-node-parent x))
        (if (eq? (rb-node-parent x) (void))
            ;; root[T] <- y
            (set-rb-tree-root! T y)
            ;; else
            (if (eq? x (node (rb-node-parent x)))
                (set-node! (rb-node-parent x) y)
                (set-contra-node! (rb-node-parent x) y)))
        (set-node! y x)
        (set-rb-node-parent! x y)))))

(define left-rotate
  (generate-rotate rb-node-left set-rb-node-left! rb-node-right set-rb-node-right!))

(define right-rotate
  (generate-rotate rb-node-right set-rb-node-right! rb-node-left set-rb-node-left!))

;;; macro taken from
;;; <http://willdonnelly.wordpress.com/2008/09/04/a-scheme-syntax-rules-primer/>
(define-syntax while
  (syntax-rules ()
    [(while condition body ...)
     (let loop ()
       (if condition
           (begin
             body ...
             (loop))
           (void)))]))

(define rb-insert
  (lambda (T z)
    (let* ([y (void)]
           [x (rb-tree-root T)])
      (while (not (eq? x (void)))
             (set! y x)
             (if (< (rb-node-value z) (rb-node-value x))
                 (set! x (rb-node-left x))
                 (set! x (rb-node-right x))))
      (set-rb-node-parent! z y)
      (if (eq? y (void))
          (set-rb-tree-root! T z)
          (if (< (rb-node-value z) (rb-node-value y))
              (set-rb-node-left! y z)
              (set-rb-node-right! y z)))
      (set-rb-node-left! z (void))
      (set-rb-node-right! z (void))
      (set-rb-node-color! z 'red)
      (rb-insert-fixup T z))))

;;; Fixes up a red black tree after one single red node has been inserted
(define rb-insert-fixup
  (lambda (T z)
    ;; y is always the 'uncle' node
    (while (eq? (rb-node-color (rb-node-parent z)) 'red)
           (if (eq? (rb-node-parent z) (rb-node-left (rb-node-parent (rb-node-parent z))))
               (let ([y (rb-node-right (rb-node-parent (rb-node-parent z)))])
                 (if (eq? (if (not (eq? y (void))) (rb-node-color y) #f) 'red)
                     (begin
                       (set-rb-node-color! (rb-node-parent z) 'black)
                       (set-rb-node-color! y 'black)
                       (set-rb-node-color! (rb-node-parent (rb-node-parent z)) 'red)
                       (set! z (rb-node-parent (rb-node-parent z))))
                     ;; else
                     (begin
                       (if (eq? z (rb-node-right (rb-node-parent z)))
                           (begin
                             (set! z (rb-node-parent z))
                             (left-rotate T z)) #f)
                       (set-rb-node-color! (rb-node-parent z) 'black)
                       (set-rb-node-color! (rb-node-parent (rb-node-parent z)) 'red)
                       (right-rotate T (rb-node-parent (rb-node-parent z))))))
               ;; else
               (let ([y (rb-node-left (rb-node-parent (rb-node-parent z)))])
                 (if (eq? (if (not (eq? y (void))) (rb-node-color y) #f) 'red)
                     (begin
                       (set-rb-node-color! (rb-node-parent z) 'black)
                       (set-rb-node-color! y 'black)
                       (set-rb-node-color! (rb-node-parent (rb-node-parent z)) 'red)
                       (set! z (rb-node-parent (rb-node-parent z))))
                     ;; else
                     (begin
                       (if (eq? z (rb-node-left (rb-node-parent z)))
                           (begin
                             (set! z (rb-node-parent z))
                             (right-rotate T z)) #f)
                       (set-rb-node-color! (rb-node-parent z) 'black)
                       (set-rb-node-color! (rb-node-parent (rb-node-parent z)) 'red)
                       (left-rotate T (rb-node-parent (rb-node-parent z)))))))
           (set-rb-node-color! (rb-tree-root T) 'black))))

;;; traverse a tree and find out which nodes are connected
(define traverse-dot-edges
  (lambda (node)
    (cond 
      [(and (eq? (rb-node-left node) (void))
             (eq? (rb-node-right node) (void)))
       '()]
      [(eq? (rb-node-left node) (void))
       (cons (list (rb-node-value node) (rb-node-value (rb-node-right node)))
               (traverse-dot-edges (rb-node-right node)))]
      [(eq? (rb-node-right node) (void))
       (cons (list (rb-node-value node) (rb-node-value (rb-node-left node)))
               (traverse-dot-edges (rb-node-left node)))]
      [else
       (append (list (list (rb-node-value node) (rb-node-value (rb-node-left node))))
               (list (list (rb-node-value node) (rb-node-value (rb-node-right node))))
               (traverse-dot-edges (rb-node-left node))
               (traverse-dot-edges (rb-node-right node)))])))

;;; traverse a tree and find out which nodes have which color
(define traverse-dot-colors
  (lambda (node)
    (cond
      [(and (eq? (rb-node-left node) (void))
             (eq? (rb-node-right node) (void)))
       (list (list (rb-node-value node) (rb-node-color node)))]
      [(eq? (rb-node-left node) (void))
       (cons (list (rb-node-value node) (rb-node-color node))
               (traverse-dot-colors (rb-node-right node)))]
      [(eq? (rb-node-right node) (void))
       (cons (list (rb-node-value node) (rb-node-color node))
               (traverse-dot-colors (rb-node-left node)))]
      [else
       (append (list (list (rb-node-value node) (rb-node-color node)))
               (traverse-dot-colors (rb-node-left node))
               (traverse-dot-colors (rb-node-right node)))])))

;;; generates a file that can be used by GraphViz' dot
(define generate-dot
  (lambda (T filename)
    (let* ([edges (traverse-dot-edges (rb-tree-root T))]
           [vertices (traverse-dot-colors (rb-tree-root T))]
           [file-port (open-output-file 
                       filename #:mode 'text #:exists 'truncate)])
      ;; create the header
      (write-string "graph {" file-port)
      (newline file-port)
      ;; write out the node configuration (red/black)
      (map (lambda (vertex)
             (write-string 
              (if (eq? (cadr vertex) 'black)
                  (format "~s[color = black, fontcolor = white, style = filled];~n" (car vertex))
                  (format "~s[color = red, fontcolor = black, style = filled];~n" (car vertex)))
                  file-port))
           vertices)
      (newline file-port)
      ;; write out the list of edges
      (map (lambda (edge)
             (write-string (format "~s -- ~s;~n" (car edge) (cadr edge)) file-port))
           edges)
      ;; write out the footer
      (write-string "}" file-port)
      (newline file-port)
      ;; close the file
      (close-output-port file-port))))

;;; sample code for trying stuff out

(define (simple-node value)
  (make-rb-node (void) (void) value (void) 'black))

(define T (make-rb-tree (simple-node 5)))
(rb-insert T (simple-node 3))
(rb-insert T (simple-node 4))

(generate-dot T "rb.dot")