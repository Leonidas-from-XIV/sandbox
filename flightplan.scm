#lang scheme
(require srfi/1)

(define default-flights
  '(("Berlin" "Rom")
    ("Berlin" "London")
    ("Berlin" "Paris")
    ("Rom" "London")
    ("London" "Madrid")
    ("London" "Athen")
    ("Paris" "London")
    ("Paris" "Madrid")
    ("Athen" "Berlin")
    ("Madrid" "Rom")))

(define start-points
  (lambda (flight-plan)
    (delete-duplicates
     (map (lambda (e)
            (car e))
          flight-plan))))

(define all-cities
  (lambda (flight-plan)
    (delete-duplicates (fold append '() flight-plan))))

(define destinations-from
  (lambda (source flight-plan)
    (map (lambda (connection)
           (cadr connection))
         (filter (lambda (connection)
                   (equal? source (car connection)))
                 flight-plan))))

(define contains?
  (lambda (lat item)
    (if (empty? lat) #f
        (if (equal? (car lat) item) #t
            (contains? (cdr lat) item)))))

(define add-hop-to-route
  (lambda (route flight-plan)
    ; get the current stop
    (let* ((current-stop (car route))
           (possible-targets (destinations-from current-stop flight-plan)))
      ; add all possible stops to the routes
      (map (lambda (target)
             (if (not (contains? route target))
                 (cons target route)
                 '()))
           possible-targets))))

(define find-routes
  (lambda (route flight-plan)
    (cond [(= (length route) (length (all-cities flight-plan))) route]
          [else (let ((next-routes (filter (lambda (e) (not (empty? e))) 
                                           (add-hop-to-route route flight-plan))))
                  (display next-routes)
                  (display "\n")
                  (if (empty? next-routes) '()
                      (map (lambda (r) (find-routes r flight-plan)) next-routes)))])))