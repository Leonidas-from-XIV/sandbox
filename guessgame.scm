#lang scheme
; Small number-guessing game

(define guess
  (lambda (computer-number)
    (display "Type in a number: ")
    (let ([user-number (string->number (read-line))])
      (cond 
        [(> user-number computer-number) (display "Too big!\n")
                                         (newline)
                                         (guess computer-number)]
        [(< user-number computer-number) (display "Too small!\n")
                                         (guess computer-number)]
        [else (display "Right!\n")]))))

(guess (random 10))