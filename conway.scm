#lang scheme

(define initial-map
  (make-immutable-hash
   '(((0 0) . dead) ((0 1) . dead) ((0 2) . dead) ((0 3) . dead) ((0 4) . dead)
     ((1 0) . dead) ((1 1) . dead) ((1 2) . alive) ((1 3) . dead) ((1 4) . dead)
     ((2 0) . dead) ((2 1) . dead) ((2 2) . alive) ((2 3) . dead) ((2 4) . dead)
     ((3 0) . dead) ((3 1) . dead) ((3 2) . alive) ((3 3) . dead) ((3 4) . dead)
     ((4 0) . dead) ((4 1) . dead) ((4 2) . dead) ((4 3) . dead) ((4 4) . dead))))

(define number-of-live-neighbors
  (lambda (matrix x y)
    0))

(define matrix-dimensions
  (lambda (matrix)
    4))

(define construct-line
  (lambda (matrix x y)
    (cond
      [(> x (matrix-dimensions matrix)) '()]
      [(eq? (hash-ref matrix (list x y)) 'dead)
       (cons "-" (construct-line matrix (add1 x) y))]
      [(eq? (hash-ref matrix (list x y)) 'alive)
       (cons "X" (construct-line matrix (add1 x) y))]
      [else (construct-line matrix (add1 x) y)])))

(define construct-block
  (lambda (matrix)
    (string-join
     (map
      (lambda (line)
        (string-join (construct-line matrix 0 line) ""))
      ;; in-range?
      '(0 1 2 3 4))
     "\n")))

(display (construct-block initial-map))