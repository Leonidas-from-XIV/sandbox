; Program that gets random numbers for lottery
; 2008 by Marek Kubica
; Kids, don't try this at home, there is enough space for improvement.

(define list-choice
  (lambda (list)
    "Chooses a random element from a list"
    (list-ref list (random (length list)))))

(define range-rec
  (lambda (start stop lat)
    (if (= start stop) '()
        (cons start (range-rec (+ start 1) stop lat)))))

;(range-rec 0 10 '())

(define range
  (lambda (start stop)
    "Returns a list starting with start and adding up to stop"
    (range-rec start stop '())))

;(range 1 (+ 49 1))

(define find-item-rec
  (lambda (lat item index)
    (if (equal? item (car lat)) index
        (find-item-rec (cdr lat) item (+ index 1)))))

(define find-item
  (lambda (lat item)
    "Finds the index of a specific item in the list"
    (find-item-rec lat item 0)))

(define remove-from-list
  (lambda (lat atom)
    (if (eq? lat '()) '()
        (if (= (car lat) atom) (remove-from-list (cdr lat) atom)
            (cons (car lat) (remove-from-list (cdr lat) atom))))))

(define lottery-numbers
  (lambda (choices to-find)
    (cond
      ((= to-find 0) '())
      (#t (let ((chosen (list-choice choices)))
            (cons chosen 
                  (lottery-numbers (remove-from-list choices chosen)
                                 (- to-find 1))))))))

(lottery-numbers (range 1 (+ 49 1)) 6)