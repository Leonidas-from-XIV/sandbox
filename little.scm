#lang scheme
;;;; Implementations of code from The Little Schemer, forth edition

;;; is the object an atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (list? x)))))

;;; is the passed object a list of atoms?
(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

;;; is the atom a member of the list of atoms?
(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? (car lat) a) #t]
      [else (member? a (cdr lat))])))

;;; remove the first occurence of the atom in the list
(define rember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

;;; get the first item of each list in the list
(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (caar l)
                  (firsts (cdr l)))])))