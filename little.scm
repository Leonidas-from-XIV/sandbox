#lang scheme
;;;; Implementations of code from The Little Schemer, forth edition

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (list? x)))))

(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? (car lat) a) #t]
      [else (member? a (cdr lat))])))

(define rember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (caar l)
                  (firsts (cdr l)))])))