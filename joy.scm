#lang scheme
;;;; Joy in Scheme, version 0.5.1
;;;; Port to PLT Scheme by Marek Kubica <marek@xivilization.net>


;;; System-dependent but essential code (currently for PLT Scheme)
(display "sysdep...")

;; SYSTEM-DEPENDENT: hash table for defines
(define joy-defines (make-hash))

;; SYSTEM-DEPENDENT: look up Joy symbol, return () if not known
(define (joy-get s)
  (hash-ref joy-defines s '()))

;; SYSTEM-DEPENDENT: set value of Joy symbol
(define (joy-set! s v)
  (hash-set! joy-defines s v))

;; SYSTEM-DEPENDENT: report error (accepts multiple arguments)
(define (joy-error . s)
  (error "joy code" (apply string-append s)))

;; SYSTEM-DEPENDENT: return an unused (or at least unusual) symbol
(define (joy-gensym)
  (gensym))


;;; The user interface
(display "ui...")

;; User interface for running Joy code (supports autoput)
(define-syntax joy
  (syntax-rules ()
    [(joy . code) (joy-exec-autoput 'code)]))

;; User interface for defining Joy symbols
(define-syntax joy-define
  (syntax-rules ()
    [(joy-define name . code) (joy-set! 'name 'code)]))

;; User interface for defining Joy modules
(define-syntax joy-module
  (syntax-rules ()
    [(joy-module name . members) (joy-module-set! 'name 'members)]))


;;; The main Joy interpreter
(display "main...")

;; The Joy stack
(define joy-stack '())

;; Write a list without its outer parentheses
(define (joy-write-list q)
  (cond
    [(null? q) (if #f #f #f)]
    [(null? (cdr q)) (write (car q)) (newline)]
    [(pair? q)
     (write (car q))
     (display #\space)
     (joy-write-list (cdr q))]
    [else (display ". ") (write q)]))

;; The Joy undefined-error flag
(define joy-undeferror #t)

;; The Joy autoput code (1 = put, 2 = put stack, else nothing)
(define joy-autoput 2)

;; Push an object on the Joy stack
(define (joy-push! x)
  (set! joy-stack (cons x joy-stack)))

;; Pop an object from the Joy stack
(define (joy-pop!)
  (if (null? joy-stack) (joy-error "Stack underflow") #f)
  (let ([x (car joy-stack)])
    (set! joy-stack (cdr joy-stack))
    x))

;; Push a list (which must be freshly consed) onto the Joy stack
(define (joy-push-list! x)
  (set! joy-stack
        (append (reverse x) joy-stack)))

;; Joy predicate for truth
(define (joy-true? x)
  (cond
    [(number? x) (not (zero? x))]
    [(eq? x #f) #f]
    [else #t]))

;; Execute a list as Joy code
(define (joy-exec c)
  (for-each joy-exec-one c))

(define (joy-exec-autoput c)
  (joy-exec c)
  (cond
    [(eqv? joy-autoput 1)
     (write (car joy-stack)) (newline)]
    [(eqv? joy-autoput 2)
     (joy-write-list joy-stack)]))

;; Lookup Joy symbol
(define (joy-lookup i)
  (let ([p (joy-get i)])
    (and
     (null? p)
     joy-undeferror
     (joy-error "Undefined symbol " (symbol->string i)))
    p))

;; Invoke a symbol or push a datum
(define (joy-exec-one i)
  (if (symbol? i)
      (joy-invoke (joy-lookup i))
      (joy-push! i)))

;; Execute a Joy quotation or call a Scheme procedure
(define (joy-invoke p)
  (cond
    [(procedure? p) (p)]
    [(pair? p) (joy-exec p)]
    [(null? p) #f]
    [else (joy-error "Attempt to invoke non-procedure")]))


;;; Module definition
(display "modules...")

;; A-list for alphatizing definition names
(define joy-alpha '())
(define joy-modstring "unknown:")

;; Alphatize a symbol
(define (joy-alphatize mode s)
  ; convert symbol s depending on mode
  (case mode
    [(private) (gensym)]
    [(public) (string->symbol (string-append
                               joy-modstring
                               (symbol->string s)))]
    [(exported) s]
    [else (joy-error (symbol->string mode) " mode unknown")]))

;; Add a redefinition to joy-alpha
(define (joy-redef! username truename)
  (set! joy-alpha (cons (cons username truename) joy-alpha)))

;; Analyze the definitions and build up joy-alpha
(define (joy-analyze! mode defs)
  (cond
    [(null? defs) (if #f #f #f)]
    [(symbol? (car defs)) (joy-analyze! (car defs) (cdr defs))]
    [else (joy-redef! (cadar defs) (joy-alphatize mode (cadar defs)))
          (joy-analyze! mode (cdr defs))]))

;; Substitute based on joy-alpha
(define (joy-subst t)
  (let ([a (assq t joy-alpha)])
    (if a (cdr a)
        (if (pair? t)
            (cons (joy-subst (car t)) (joy-subst (cdr t)))
            t))))

;; Install a module definition
(define (joy-install! def)
  (if (pair? def) (joy-set! (cadr def) (cddr def)) #f))

;; Install module
(define (joy-module-set! name members)
  (set! joy-alpha '())
  (set! joy-modstring (string-append (symbol->string name) ":"))
  (joy-analyze! 'public members)
  (for-each joy-install! (joy-subst members)))


;;; Macros for defining Joy primitives
(display "macros...")

;; Push one result
(define-syntax joy-prim
  (syntax-rules ()
    [(joy-prim (name . vars) . code)
     (joy-set! 'name (lambda ()
                       (joy-let vars (joy-push! (begin . code)))))]))

;; Push a freshly consed list of results
(define-syntax joy-prim-list
  (syntax-rules ()
    [(joy-prim-list (name . vars) . code)
     (joy-set! 'name (lambda ()
                       (joy-let vars (joy-push-list! (begin . code)))))]))

;; Push nothing
(define-syntax joy-prim-void
  (syntax-rules ()
    [(joy-prim-void (name . vars) . code)
     (joy-set! 'name (lambda ()
                       (joy-let vars (begin . code))))]))

;; Set up appropriate pops
(define-syntax joy-let
  (syntax-rules ()
    [(joy-let () . body) 
     (begin . body)]
    [(joy-let (x1 x2 ...) . body)
     (joy-let (x2 ...) (let ((x1 (joy-pop!))) . body))]))


;;; Joy non-combinator primitives
(display "prims...")

;; Simple niladic primitives
(joy-prim (false) #f)
(joy-prim (true) #t)
(joy-prim (maxint) #f)
(joy-prim (setsize) #f)
(joy-prim (stack) joy-stack)
(joy-prim (autoput) joy-autoput)
(joy-prim (undeferror) (if joy-undeferror 1 0))
(joy-prim (stdin) (current-input-port))
(joy-prim (stdout) (current-output-port))

;; Simple operators
(joy-prim-void (id) #f)
(joy-prim-list (dup x) (list x x))
(joy-prim-list (swap x y) (list y x))
(joy-prim-list (rollup x y z) (list z x y))
(joy-prim-list (rolldown x y z) (list y z x))
(joy-prim-list (rotate x y z) (list z y x))
(joy-prim (popd y z) z)
(joy-prim-list (dupd y z) (list y y z))
(joy-prim-list (swapd x y z) (list y x z))
(joy-prim-list (rollupd x y z w) (list z x y w))
(joy-prim-list (rolldownd x y z w) (list y z x w))
(joy-prim-list (rotated x y z w) (list z y x w))
(joy-prim-void (pop x) #f)
(joy-prim (choice b t f) (if b t f))

;; Logical primitives (FIXME: don't handle sets yet)
(joy-prim (or x y) (or x y))
(joy-prim (xor x y) (eq? x (not y)))
(joy-prim (and x y) (and x y))
(joy-prim (not x) (not x))

;; Arithmetic primitives
(joy-prim (+ i j) (+ i j))
(joy-prim (- i j) (- i j))
(joy-prim (* i j) (* i j))
(joy-prim (/ i j) (/ i j))
(joy-prim (rem i j) (remainder i j))
(joy-prim (div i j) (list (quotient i j) (remainder i j)))
(joy-prim (sign i) (if (negative? i) -1 (if (zero? i) 0 1)))
(joy-prim (neg i) (- i))
(joy-prim (ord c) (char->integer c))
(joy-prim (chr i) (integer->char i))
(joy-prim (abs n) (abs n))
(joy-prim (pred n) (- n 1))
(joy-prim (succ n) (+ n 1))
(joy-prim (max m n) (max m n))
(joy-prim (min m n) (max m n))


;; Transcendental primitives
(joy-prim (acos f) (acos f))
(joy-prim (asin f) (asin f))
(joy-prim (atan f) (atan f))
(joy-prim (atan2 f g) (atan f g))
(joy-prim (ceil f) (ceiling f))
(joy-prim (cos f) (cos f))
(joy-prim (cosh f) (cosh f))	; SYSTEM-DEPENDENT
(joy-prim (exp f) (exp f))
(joy-prim (floor f) (floor f))
(joy-prim (log f) (log f))
(joy-prim (log10 f) (/ (log f) (log 10)))
(joy-prim (pow f g) (expt f g))
(joy-prim (sin f) (sin f))
(joy-prim (sinh f) (sinh f))	; SYSTEM-DEPENDENT
(joy-prim (sqrt f) (sqrt f))
(joy-prim (tan f) (tan f))
(joy-prim (tanh f) (tanh f))	; SYSTEM-DEPENDENT
(joy-prim (trunc f) (truncate f))

;; Date primitives are system-dependent and not implemented

;; String/numeric conversion primitives
(joy-prim (strtol s i) (string->number s i))
(joy-prim (strtod s) (string->number s))
(joy-prim (format n i) (number->string n i))	; different from C-Joy
; formatf not implemented

;; Random number primitives are system-dependent and not implemented

;; Simple I/O primitives
(joy-prim (get) (read))
(joy-prim-void (put x) (write x))
(joy-prim-void (putchar c) (display (integer->char c)))
(joy-prim-void (putchars x) (display x))
(joy-prim-void (include s) (load (string-append s ".ss")))	; SYSTEM-DEPENDENT

;; Stream primitives
(joy-prim-void (fclose f)
               (if (input-port? f) (close-input-port f)
                   (close-output-port f)))
(joy-prim (eof x) (eof-object? x))	; different from C-Joy
(joy-prim (fgetch) (read-char (car joy-stack)))
(joy-prim (fopen p m)
          (cond
            [(string=? m "r") (open-input-file p)]
            [(string=? m "w") (open-output-file p)]
            [else (joy-error "Invalid fopen mode " m)]))
(joy-prim (fput x) (write x (car joy-stack)))
(joy-prim (fputch c) (display c (car joy-stack)))
(joy-prim (fputchars s) (display s (car joy-stack)))


;; Replace the stack with its topmost member
(joy-prim-void (unstack x) (set! joy-stack x))

;; Cons element onto aggregate
(joy-prim (cons x a)
          (if (string? a)
              (string-append (string x) a)
              (cons x a)))

;; Swapped cons
(joy-prim (swons a x)
          (if (string? a)
              (string-append (string x) a)
              (cons x a)))

;; Get first element
(joy-prim (first a)
          (if (string? a)
              (string-ref a 0)
              (car a)))

;; Get remaining elements
(joy-prim (rest a)
          (if (string? a)
              (substring a 1 (string-length a))
              (cdr a)))

;; FIXME: compare not implemented

;; Element of aggregate at location (zero-based)
(joy-prim (at a i)
          (if (string? a)
              (string-ref a i)
              (list-ref a i)))

;; Inverse of at
(joy-prim (of i a)
          (if (string? a)
              (string-ref a i)
              (list-ref a i)))

;; Size of aggregate
(joy-prim (size a)
          (if (string? a)
              (string-length a)
              (length a)))

;; FIXME: opcase not implemented

;; FIXME: case not implemented

;; Uncons an aggregate
(joy-prim-list (uncons a)
               (if (string? a)
                   (list (string-ref a 0) (substring a 0 (string-length a)))
                   (list (car a) (cdr a))))

;; Uncons an aggregate and swap
(joy-prim-list (unswons a)
               (if (string? a)
                   (list (substring a 0 (string-length a)) (string-ref a 0))
                   (list (cdr a) (car a))))

;; Drop first n elements of an aggregate
(joy-prim (drop a n)
          (if (string? a)
              (substring a n (string-length a))
              (list-tail a n)))

;; Take first n elements of aggregate
(joy-prim (take a n)
          (if (string? a)
              (substring a 0 n)
              (reverse (joy-reversed-head a n))))
(define (joy-reversed-head a n)
  (if (or (zero? n) (null? a))
      '()
      (cons (car a) (joy-reversed-head (cdr a) (- n 1)))))

;; Concatenate aggregates
(joy-prim (concat s t)
          (if (string? s) (string-append s t) (append s t)))

;; Concatenate aggregates with an element in the middle
(joy-prim (enconcat x s t)
          (if (string? s)
              (string-append s (string x) t)
              (append s (list x) t)))

;; Symbol/string conversion
(joy-prim (name s) (symbol->string s))
(joy-prim (intern s) (string->symbol s))
(joy-prim (body u) (joy-get u))

;; Null aggregate or zero number
(joy-prim (null x)
          (cond
            ((string? x) (zero? (string-length x)))
            ((null? x) #t)
            (else (zero? x))))

;; Small aggregate or zero or one number
(joy-prim (small x)
          (cond
            [(string? x) (<= (string-length x) 1)]
            [(null? x) #t]
            [(pair? x) (null? (cdr x))]
            [else (<= 0 x 1)]))

;; Relational operators

(joy-prim (= x y)
          (cond
            [(symbol? x)
             (string=? (symbol->string x) (symbol->string y))]
            [(string? x)
             (string=? x y)]
            [else (= x y)]))

(joy-prim (!= x y)
          (cond
            [(symbol? x)
             (not (string=? (symbol->string x) (symbol->string y)))]
            [(string? x)
             (not (string=? x y))]
            [else (not (= x y))]))

(joy-prim (< x y)
          (cond
            [(symbol? x)
             (string<? (symbol->string x) (symbol->string y))]
            [(string? x)
             (string<? x y)]
            [else (= x y)]))

(joy-prim (> x y)
          (cond
            [(symbol? x)
             (string>? (symbol->string x) (symbol->string y))]
            [(string? x)
             (string>? x y)]
            [else (> x y)]))

(joy-prim (<= x y)
          (cond
            [(symbol? x)
             (string<=? (symbol->string x) (symbol->string y))]
            [(string? x)
             (string<=? x y)]
            [else (<= x y)]))

(joy-prim (>= x y)
          (cond
            [(symbol? x)
             (string>=? (symbol->string x) (symbol->string y))]
            [(string? x)
             (string>=? x y)]
            [else (>= x y)]))

;; Tree equality
(joy-prim (equal t u) (equal? t u))

;; Membership
(joy-prim (has a x) (if (string? a) (joy-stringmem a x) (memq a x)))
(joy-prim (in x a) (if (string? a) (joy-stringmem a x) (memq a x)))
(define (joy-stringmem s c)
  (define (try i r)
    (cond
      [(zero? r) #f]
      [(eqv? c (string-ref s i)) #t]
      [else (joy-stringmem (+ i 1) (- r 1))]))
  (try 0 (string-length s)))

;; Type predicates
(joy-prim (integer x) (integer? x))
(joy-prim (char x) (char? x))
(joy-prim (logical x) (boolean? x))
(joy-prim (set x) (list? x))
(joy-prim (string x) (string? x))
(joy-prim (list x) (or (pair? x) (null? x)))
(joy-prim (leaf x) (not (or (pair? x) (null? x))))
(joy-prim (float x) (real? x))
(joy-prim (user x) (and (symbol? x) (pair? (joy-get x))))
(joy-prim (file x) (or (input-port? x) (output-port? x)))

;; Environment manipulation
(joy-prim-void (setundeferror n) (set! joy-undeferror (joy-true? n)))
(joy-prim-void (setautoput n) (set! joy-autoput n))


;;; Joy combinator primitives
(display "combs...")

;; Evaluate thunk on a stabilized stack
(define (joy-stable p)
  (let*
      ([s joy-stack]
       [r (p)])
    (set! joy-stack s)
    r))

;; Execute Joy quotation stably, return top of stack
(define (joy-stable-exec p) (joy-stable (lambda () (joy-exec p) (joy-pop!))))
 
;; Return truth value of stabilized execution
(define (joy-yields-true? p) (joy-true? (joy-stable-exec p)))

;; Simple combinators
(joy-prim-void (i x) (joy-exec x))
(joy-prim-void (x) (joy-exec (car joy-stack)))
(joy-prim (dip x p) (joy-exec p) x)

;; app1 app11 app12

;; Construct combinator
(joy-prim-void (construct p1 p2)
               (joy-stable (lambda ()
                             (joy-exec p1)
                             (for-each (lambda (q) (joy-exec q) (joy-pop!)) p2))))

;; N-ary combinators
(joy-prim (nullary p) (let ((r (joy-stable-exec p))) r))
(joy-prim (unary p) (let ((r (joy-stable-exec p))) (joy-pop!) r))
(joy-prim (binary p) (let ((r (joy-stable-exec p))) (joy-pop!) (joy-pop!) r))
(joy-prim (ternary p) (let ((r (joy-stable-exec p))) (joy-pop!) (joy-pop!) (joy-pop!) r))

;; Execute unary combinator twice
(joy-prim-list (unary2 x1 x2 p)
               (let*
                   ([r1 (begin (joy-push! x1) (joy-exec p) (joy-pop!))]
                    [r2 (begin (joy-push! x2) (joy-exec p) (joy-pop!))])
                 (list r1 r2)))

(joy-set! 'app2 (joy-get 'unary2))

;; Execute unary combinator three times
(joy-prim (unary3 x1 x2 x3 p)
          (let*
              ([r1 (begin (joy-push! x1) (joy-exec p) (joy-pop!))]
               [r2 (begin (joy-push! x2) (joy-exec p) (joy-pop!))]
               [r3 (begin (joy-push! x3) (joy-exec p) (joy-pop!))])
            (list r1 r2 r3)))

(joy-set! 'app3 (joy-get 'unary3))

;; Execute unary combinator four times
(joy-prim (unary4 x1 x2 x3 x4 p)
          (let*
              ([r1 (begin (joy-push! x1) (joy-exec p) (joy-pop!))]
               [r2 (begin (joy-push! x2) (joy-exec p) (joy-pop!))]
               [r3 (begin (joy-push! x3) (joy-exec p) (joy-pop!))]
               [r4 (begin (joy-push! x4) (joy-exec p) (joy-pop!))])
            (list r1 r2 r3 r4)))

(joy-set! 'app4 (joy-get 'unary4))

;; Cleave combinator
(joy-prim-list (cleave p1 p2)
               (let*
                   ([r1 (joy-stable-exec p1)]
                    [r2 (joy-stable-exec p2)])
                 (joy-pop!)
                 (list r1 r2)))

;; Conditional combinators
(joy-prim-void (branch p t e)
               (if (joy-true? p) (joy-exec t) (joy-exec e)))
(joy-prim-void (ifte p t e)
               (if (joy-yields-true? p) (joy-exec t) (joy-exec e)))
(joy-prim-void (ifinteger x t e) (if (integer? x) (joy-exec t) (joy-exec e)))
(joy-prim-void (ifchar x t e) (if (char? x) (joy-exec t) (joy-exec e)))
(joy-prim-void (iflogical x t e) (if (boolean? x) (joy-exec t) (joy-exec e)))
(joy-prim-void (ifset x t e) (if (list? x) (joy-exec t) (joy-exec e)))
(joy-prim-void (ifstring x t e) (if (string? x) (joy-exec t) (joy-exec e)))
(joy-prim-void (iflist x t e)
               (if (or (pair? x) (null? x)) (joy-exec t) (joy-exec e)))
(joy-prim-void (iffloat x t e) (if (real? x) (joy-exec t) (joy-exec e)))
(joy-prim-void (iffile x t e)
               (if (or (input-port? x) (output-port? x)) (joy-exec t) (joy-exec e)))

;; Joy's version of cond
(joy-prim-void (cond p) (joy-cond p))
(define (joy-cond p)
  (cond
    [(null? p) #f]
    [(null? (cdr p)) (joy-exec (car p))]
    [(joy-yields-true? (caar p)) (joy-exec (cdar p))]
    [else (joy-cond (cdr p))]))

;; While-do combinator
(joy-prim-void (while p q) (joy-while p q))
(define (joy-while p q)
  (when (joy-yields-true? p)
    (joy-exec q)
    (joy-while p q)))

;; Linear recursion combinator
(joy-prim-void (linrec p t r1 r2) (joy-linrec p t r1 r2))
(define (joy-linrec p t r1 r2)
  (cond
    [(joy-yields-true? p) (joy-exec t)]
    [else (joy-exec r1) (joy-linrec p t r1 r2) (joy-exec r2)]))

;; Tail recursion combinator
(joy-prim-void (tailrec p t r) (joy-tailrec p t r))
(define (joy-tailrec p t r)
  (cond
    [(joy-yields-true? p) (joy-exec t)]
    [else (joy-exec r) (joy-tailrec p t r)]))

;; Binary recursion combinator
(joy-prim-void (binrec p t r1 r2) (joy-binrec p t r1 r2))
(define (joy-binrec p t r1 r2)
  (cond
    [(joy-yields-true? p) (joy-exec t)]
    [else (joy-exec r1)
          (let* ([n2 (joy-pop!)] [n1 (joy-pop!)])
            (joy-push! n1)
            (joy-binrec p t r1 r2)
            (joy-push! n2)
            (joy-binrec p t r1 r2)
            (joy-exec r2))]))

;; General recursion combinator
(joy-prim-void (genrec p t r1 r2)
               (cond
                 [(joy-yields-true? p) (joy-exec t)]
                 [else
                  (joy-exec r1)
                  (joy-push! (list p t r1 r2 'genrec))
                  (joy-exec r2)]))

;; FIXME: condlinrec

(joy-prim-void (step a p)
               (if (string? a)
                   (joy-step-string a p 0 (string-length a))
                   (joy-step-list a p)))

(define (joy-step-string s p i n)
  (cond
    [(zero? n) #f]
    [else
     (joy-stable (lambda ()
                   (joy-push! (string-ref s i))
                   (joy-exec p)))
     (joy-step-string s p (+ i 1) (- n 1))]))

(define (joy-step-list a p)
  (for-each (lambda (e)
              (joy-stable (lambda ()
                            (joy-push! e)
                            (joy-exec p)))) a))


;; FIXME: fold

;; Map aggregate through quotation
(define joy-map-result '())
(joy-prim-void (map a p)
               (cond
                 [(string? a)
                  (let ((len (string-length a)))
                    (set! joy-map-result (make-string len))
                    (joy-map-string! a p 0 len))]
                 [else
                  (set! joy-map-result '())
                  (joy-map-list! a p)])
	(joy-push! joy-map-result))

(define (joy-map-string! s p i n)
  (cond
    [(zero? n) #f]
    [else
     (string-set! joy-map-result i
                  (joy-stable (lambda ()
                                (joy-push! (string-ref s i))
                                (joy-exec p))))
     (joy-map-string! s p (+ i 1) (- n 1))]))

(define (joy-map-list! a p)
  (cond
    [(null? a) #f]
    [else
     (set! joy-map-result (cons (joy-stable (lambda ()
                                              (joy-push! (car a))
                                              (joy-exec p))) joy-map-result))
     (joy-map-list! (cdr a) p)]))


;; Execute N times combinator
(joy-prim-void (times n p) (joy-times n p))
(define (joy-times n p)
  (cond
    [(zero? n) #f]
    [else (joy-exec p) (joy-times (- n 1) p)]))

;; Infra-stack combinator
(joy-prim (infra l p)
          (let ([s joy-stack])
            (set! joy-stack l)
            (joy-exec p)
            (let ([r joy-stack])
              (set! joy-stack s)
              r)))

;; FIXME: filter, split, some, all

;; FIXME: treestep, treerec, treegenrec

;; FIXME: need to do something for manual (doc strings?)

;; System access is system-dependent and not included here.


;;; The Joy integrator
(display "integrator...")

(joy-prim-void (integrate words)
               (for-each (lambda (w)
                           (joy-set! w (joy-integrate w '()))) words))

(define (joy-integrate word parents)
  (cond
    [(memq word parents) word]
    [(symbol? word) (joy-integrate-sym word parents)]
    [(pair? word) (cons
                   (joy-integrate (car word) parents)
                   (joy-integrate (cdr word) parents))]
    [else word]))

(define (joy-integrate-sym s parents)
  (let ([v (joy-get s)])
    (if (pair? v)
        (joy-integrate v (cons s parents))
        s)))


;;; REPL
(display "repl...")

;; Read-exec-print loop
(define (joy-repl)
  (joy-exec-autoput (read))
  (joy-repl))

;;; Done
(display "done\n")
(joy-repl)