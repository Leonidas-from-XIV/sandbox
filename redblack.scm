#lang scheme
;;;; Implementation of red black trees
;;;; 2009 by Marek Kubica <marek@xivilization.net>
;;;; This code is free under the MIT License
;;;;
;;;; The implementation is taken from CLRS 2nd Edition and adapted for 
;;;; PLT Scheme 4.
;;;;
;;;; A good applet demonstrating the algorithm as in CLRS can be found at
;;;; <http://people.ksp.sk/~kuko/bak/>

;;; define the representation of one single node
(define-struct rb-node
  (left right value parent color) #:mutable)

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

;;; inserts the node z into the binary tree and calls the fixup afterwards
;;; to make sure it is still a red black tree
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

(define generate-fixup-branch
  (lambda (node-access node-rotate contra-node-rotate)
    (lambda (T z)
      (let ([y (node-access (rb-node-parent (rb-node-parent z)))])
                 (if (eq? (if (not (eq? y (void))) (rb-node-color y) #f) 'red)
                     (begin
                       (set-rb-node-color! (rb-node-parent z) 'black)
                       (set-rb-node-color! y 'black)
                       (set-rb-node-color! (rb-node-parent (rb-node-parent z)) 'red)
                       (set! z (rb-node-parent (rb-node-parent z))))
                     ;; else
                     (begin
                       (if (eq? z (node-access (rb-node-parent z)))
                           (begin
                             (set! z (rb-node-parent z))
                             (contra-node-rotate T z)) #f)
                       (set-rb-node-color! (rb-node-parent z) 'black)
                       (set-rb-node-color! (rb-node-parent (rb-node-parent z)) 'red)
                       (node-rotate T (rb-node-parent (rb-node-parent z))))))
      ;; return the new z, since we need this in the calling code
      z)))

;;; Fixes up a red black tree after one single red node has been inserted
(define rb-insert-fixup
  (lambda (T z)
    ;; y is always the 'uncle' node
    (while (and (not (eq? (rb-node-parent z) (void)))
                (eq? (rb-node-color (rb-node-parent z)) 'red))
           (if (eq? (rb-node-parent z) (rb-node-left (rb-node-parent (rb-node-parent z))))
               ;; call the branch for the "left" case
               (set! z ((generate-fixup-branch rb-node-right right-rotate left-rotate) T z))
               ;; else, the "right" case
               (set! z ((generate-fixup-branch rb-node-left left-rotate right-rotate) T z))))
    (set-rb-node-color! (rb-tree-root T) 'black)))

(define tree-minimum
  (lambda (x)
    (while (not (eq? (rb-node-left x) (void)))
           (set! x (rb-node-left x)))
    x))

(define tree-successor
  (lambda (x)
    (if (not (eq? (rb-node-right x) (void)))
        (tree-minimum (rb-node-right x))
        ;; else
        (let ([y (rb-node-parent x)])
          (while (and (not (eq? y (void))) (eq? x (rb-node-right y)))
                 (set! x y)
                 (set! y (rb-node-parent y)))
          y))))

(define rb-delete-fixup
  (lambda (T x)
    (while (and 
            (not (eq? x (rb-tree-root T)))
            (eq? (rb-node-color x) 'black))
           (if (eq? x (rb-node-left (rb-node-parent x)))
               (let ([w (rb-node-right (rb-node-parent x))])
                 (if (eq? (rb-node-color w) 'red)
                     (begin
                       (set-rb-node-color! w 'black)
                       (set-rb-node-color! (rb-node-parent x) 'red)
                       (left-rotate T (rb-node-parent x))
                       (set! w (rb-node-right (rb-node-parent x))))
                     #f)
                 (if (and
                      (eq? (rb-node-color (rb-node-left w)) 'black)
                      (eq? (rb-node-color (rb-node-right w)) 'black))
                     (begin
                       (set-rb-node-color! w 'red)
                       (set! x (rb-node-parent x)))
                     (begin
                       (if (eq? (rb-node-color (rb-node-right w)) 'black)
                           (begin
                             (set-rb-node-color! (rb-node-left w) 'black)
                             (set-rb-node-color! w 'rot)
                             (right-rotate T w)
                             (set! w (rb-node-right (rb-node-parent x))))
                           #f)
                       (set-rb-node-color! w (rb-node-color (rb-node-parent x)))
                       (set-rb-node-color! (rb-node-parent x) 'black)
                       (set-rb-node-color! (rb-node-right w) 'black)
                       (left-rotate T (rb-node-parent x))
                       (set! x (rb-tree-root T)))))
               ;; else
               "TODO"))
    (set-rb-node-color! x 'black)))

(define rb-delete
  (lambda (T z)
    (let* ([y (void)]
           [x (void)])
      (if (or (eq? (rb-node-left z) (void))
              (eq? (rb-node-right z) (void)))
          (set! y z)
          (set! y (tree-successor z)))
      (if (not (eq? (rb-node-left y) (void)))
          (set! x (rb-node-left y))
          (set! x (rb-node-right y)))
      ;; if x is void, we don't need to set its parent
      (if (not (eq? x (void)))
          (set-rb-node-parent! x (rb-node-parent y))
          #f)
      (if (eq? (rb-node-parent y) (void))
          (set-rb-tree-root! T x)
          ;; else
          (if (eq? y (rb-node-left (rb-node-parent y)))
              (set-rb-node-left! (rb-node-parent y) x)
              (set-rb-node-right! (rb-node-parent y) x)))
      (if (not (eq? y z))
          (begin
            (set-rb-node-value! z (rb-node-value y))
            (set-rb-node-left! z (rb-node-left y))
            (set-rb-node-right! z (rb-node-right y)))
          #f)
      (if (eq? (rb-node-color y) 'black)
          (rb-delete-fixup T x)
          (void)))))

;;; traverse a tree and find out which nodes are connected
(define traverse-dot-edges
  (lambda (node)
    (cond
      ;; no child nodes
      [(and (eq? (rb-node-left node) (void))
             (eq? (rb-node-right node) (void)))
       '()]
      ;; just a right child node
      [(eq? (rb-node-left node) (void))
       (cons (list (rb-node-value node) (rb-node-value (rb-node-right node)))
               (traverse-dot-edges (rb-node-right node)))]
      ;; just a left child node
      [(eq? (rb-node-right node) (void))
       (cons (list (rb-node-value node) (rb-node-value (rb-node-left node)))
               (traverse-dot-edges (rb-node-left node)))]
      ;; both left and right child nodes exist
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

(define first (simple-node 5))
(define second (simple-node 3))
(define third (simple-node 4))
(define fourth (simple-node 6))
(define fifth (simple-node 2))

(define T (make-rb-tree first))
(rb-insert T second)
(rb-insert T third)
(rb-insert T fourth)
(rb-insert T fifth)
(generate-dot T "rb.dot")
(rb-delete T second)
;(generate-dot T "rb.dot")