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
                   (if (equal? source (car connection)) #t #f))
                 flight-plan))))              

(define find-route
  (lambda (flight-plan)
    #f))