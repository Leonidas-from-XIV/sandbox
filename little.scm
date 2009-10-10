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

(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old
             (cons new (cdr lat)))]
      [else (cons (car lat)
                  (insertR new old
                           (cdr lat)))])))

(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new lat)]
      [else (cons (car lat)
                  (insertL new old
                           (cdr lat)))])))

(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons new (cdr lat))]
      [else (cons (car lat)
                  (subst new old (cdr lat)))])))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [(or (eq? (car lat) o1) (eq? (car lat) o2))
       (cons new (cdr lat))]
      [else (cons (car lat)
                  (subst2 new o1 o2 (cdr lat)))])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (multirember a (cdr lat))]
      [else (cons (car lat)
                  (multirember a (cdr lat)))])))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old
             (cons new (multiinsertR new old (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertR new old
                           (cdr lat)))])))

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertL new old
                           (cdr lat)))])))

(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
      [else (cons (car lat) (multisubst new old (cdr lat)))])))

(define o+
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (o+ n (sub1 m)))])))

(define o-
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (sub1 (o- n (sub1 m)))])))