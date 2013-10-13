;;; an infinite sequence returning fibonacci numbers
;;; `iterate` rocks
(def fib-seq
  (map second
       (cons [0 0]
             (iterate
               (fn [[a b]] [b (+ a b)])
               [0 1]))))

(take 10 fib-seq)
(drop 30 fib-seq)
