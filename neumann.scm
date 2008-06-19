;; Middle-square method calculation in Scheme
;; 2008 by Marek Kubica
(require (lib "kw.ss"))

;; get a random number that is in [min; max[
(define random-with-offset
  (lambda (min max)
    (+ (random (- max min)) min)))

;; get a random number that is n digits long
(define n-digit-random-number
  (lambda (n)
    (let ((largest (expt 10 n))
          (tiniest (expt 10 (- n 1))))
      (random-with-offset tiniest largest))))

;; get n middle letters
(define middle-letters
  (lambda (string n)
    (let* ((len (string-length string))
             (start (floor (- (/ len 2) (/ n 2)))))
      (substring string start (+ start n)))))

;; one single neumann calculation step
(define neumann-step
  (lambda (seed n)
    (string->number
     (middle-letters (number->string (expt seed 2)) n))))

;; you can define a seed or you can let this function create
;; a seed on its own. x is the number of steps desired
(define neumann-prng
  (lambda/kw (#:optional n x [seed (n-digit-random-number n)])
    (let ((next-seed (neumann-step seed n)))
      (cond [(= x 1) next-seed]
            [else (neumann-prng n (- x 1) next-seed)]))))

;; Wikipedia uses 62 as initial seed
(neumann-prng 2 2 62)
;; retry with a random seed
(neumann-prng 2 2)