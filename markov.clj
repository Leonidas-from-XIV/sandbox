(ns markov
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [clojure.string :refer [split join]])
  (:require [clojure.java.io :refer [reader]])
  (:gen-class))

(defn create-markov-transitions [text]
  (set (partition 3 1 text)))

(defn transition-cache [cache triplet]
  (let [[first second third] triplet
        key [first second]]
    (update-in cache (list key) (fn [entries] (conj entries third)))))

(defn markov-link [cache words]
  (rand-nth (cache words)))

(defn markov-chain
  ([words]
   (let [seed (rand-int (- (count words) 3))
         seed-first (nth words seed)
         seed-second (nth words (inc seed))
         ; create the cache automatically
         cache (reduce transition-cache {} (create-markov-transitions words))]
     (markov-chain cache seed-first seed-second)))
  ([cache word1 word2]
   (let [new-word (markov-link cache [word1 word2])]
     (lazy-seq
       (cons new-word (markov-chain cache word2 new-word))))))

(def cli-options
  [["-n" "--number" "Number of words to generate"
    :default 25
    :parse-fn #(Integer/parseInt %)]])

(defn -main [& args]
  (let [{{number :number} :options} (parse-opts args cli-options)]
    (let [words (mapcat #(remove empty? (split % #"\s"))
                        (line-seq (reader System/in)))
          generated-words (take number (markov-chain words))]
      (println (join " " generated-words)))))

(try (require 'leiningen.exec)
     (when @(ns-resolve 'leiningen.exec '*running?*)
       (apply -main (rest *command-line-args*)))
     (catch java.io.FileNotFoundException e))
