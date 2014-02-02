(ns fizzbuzz
  (:require [clojure.string :refer [split split-lines join]]
            [clojure.java.io :refer [reader]]))

(defn get-a-b-n [line]
  (map #(Integer/parseInt % 10) (split line #"\s")))

(defn divisible? [num factor]
  (= (mod num factor) 0))

(defn divide [num a b]
  (cond
    (and (divisible? num a) (divisible? num b)) "FB"
    (divisible? num a) "F"
    (divisible? num b) "B"
    :else (str num)))

(defn process-line [a b n]
  (for [num (range 1 (inc n))]
    (divide num a b)))

(defn -main [& args]
  (let [input-file (nth args 0)]
    (with-open [rdr (reader input-file)]
      (doseq [line (line-seq rdr)]
        (println (join " " (apply process-line (get-a-b-n line))))))))

(apply -main *command-line-args*)
