(use '[clojure.contrib.io :only (read-lines)]
     '[clojure.contrib.string :only (split)]
     '[clojure.contrib.str-utils :only (str-join)]
     '[clojure.contrib.command-line :only (with-command-line)])

(defn create-markov-transitions [text]
  (set (partition 3 1 text)))

(defn transition-cache [cache triplet]
  (let [[first second third] triplet
        key [first second]]
    (update-in cache (list key) (fn [entries] (conj entries third)))))

(def words
  (mapcat #(remove empty? (split #"\s" %))
    (read-lines System/in)))

(defn markov-link [cache words]
  (rand-nth (cache words)))

(defn markov-chain
  ([cache]
   (let [seed (rand-int (- (count words) 3))
        seed-first (nth words seed)
        seed-second (nth words (inc seed))]
     (markov-chain cache seed-first seed-second)))
  ([cache word1 word2]
   (let [new-word (markov-link cache [word1 word2])]
     (lazy-seq
       (cons new-word (markov-chain cache word2 new-word))))))

(def cache (reduce transition-cache {} (create-markov-transitions words)))

(with-command-line
  *command-line-args*
  "Usage: blah"
  [[words w "The number of words to generate" 25]]
  (let [generated-words (take 25 (markov-chain cache))]
    (println (str-join " " generated-words))))

