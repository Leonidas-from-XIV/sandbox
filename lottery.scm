; Gets n random numbers for lottery
; 2008 by Marek Kubica
; Don't be surprised if there is strange stuff, this is just for
; autoeducational purposes

(require (lib "kw.ss"))

(define list-choice
  (lambda (list)
    "Chooses a random element from a list"
    (list-ref list (random (length list)))))

(define range
  (lambda/kw (#:optional start stop [lat '()])
    "Returns a list starting with start and adding up to stop"
    (if (= start stop) '()
        (cons start (range (+ start 1) stop lat)))))

;(range 0 10)

; funnily enough, The Litte Schemer defines a nearly identical function like
; this one - called `rember` (p. 41)
(define remove-from-list
  (lambda (lat atom)
    "Removes an item from a list"
    (if (null? lat) '()
        (if (equal? (car lat) atom) (remove-from-list (cdr lat) atom)
            (cons (car lat) (remove-from-list (cdr lat) atom))))))

(define lottery-numbers
  (lambda (choices to-find)
    "Chooes to-find unique numbers from choices"
    (if (= to-find 0) '()
        (let ((chosen (list-choice choices)))
          (cons chosen
                (lottery-numbers (remove-from-list choices chosen)
                                 (- to-find 1)))))))

(lottery-numbers (range 1 (+ 49 1)) 6)