(ns one-times-one
  (:use [clojure.contrib.combinatorics :only (cartesian-product)]))

(let [maximum 10
      numbers (range 1 (+ maximum 1))
      combinations (cartesian-product numbers numbers)]
  (doseq [[a b] combinations]
    (printf "%d * %d = %d\n" a b (+ a b))))
