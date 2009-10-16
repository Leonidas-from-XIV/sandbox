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

(define addtup
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (o+ (car l) (addtup (cdr l)))])))

(define o*
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (o+ n (o* n (sub1 m)))])))

(define tup+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons
             (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))])))

(define o>
  (lambda (n m)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (o> (sub1 n) (sub1 m))])))

(define o<
  (lambda (n m)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (o< (sub1 n) (sub1 m))])))

(define o=
  (lambda (n m)
    (not (or (o< n m) (o> n m)))))

(define oexpt
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (o* n (oexpt n (sub1 m)))])))

(define o/
  (lambda (n m)
    (cond
      [(o< n m) 0]
      [else (add1 (o/ (o- n m) m))])))

(define length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (length (cdr lat)))])))

(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(define rempick
  (lambda (n lat)
    (cond
      [(one? n) (cdr lat)]
      [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))])))

(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(not (number? (car lat))) (all-nums (cdr lat))]
      [else (cons (car lat) (all-nums (cdr lat)))])))

(define eqan?
  (lambda (e1 e2)
    (cond
      [(and (number? e1) (number? e2)) (= e1 e2)]
      [(or (number? e1) (number? e2)) #f]
      [else (eq? e1 e2)])))

(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eqan? (car lat) a) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))

(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define rember*
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))]
      [(eq? (car l) a) (rember* a (cdr l))]
      [else (cons (car l) (rember* a (cdr l)))])))

(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(list? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l)))]
      [(eq? (car l) old) (cons old (cons new (insertR* new old (cdr l))))]
      [else (cons (car l) (insertR* new old (cdr l)))])))
