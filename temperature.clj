(defn raw-input
  ([] (raw-input ""))
  ([prompt]
   (if (not (empty? prompt))
     (do 
       (print prompt)
       (flush)))
   (read-line)))

(defn read-number
  ([] (read-number ""))
  ([prompt]
   (let [value (raw-input prompt)
        number (try
                 (Integer/parseInt value 10)
                 (catch NumberFormatException e nil))]
    (if (not (nil? number)) number
      (do
        (println "Not a number")
        (recur prompt))))))

(defn get-range []
  (let [start (read-number "Enter start: ")
        stop (read-number "Enter stop: ")
        step 10]
    (range start (+ stop 1) step)))

(defn celsius->kelvin [temperature]
  (+ temperature 273.15))

(defn celsius->fahrenheit [temperature]
  (-> temperature
    (* 9)
    (/ 5)
    (+ 32)))

(defn convert [value]
  [value (celsius->kelvin value) (celsius->fahrenheit value)])

(defn tabellize [items]
  (doseq [[celsius kelvin fahrenheit] items]
    (printf "C: %s | K: %.2f | F: %.1f\n" celsius kelvin (float fahrenheit))))

(tabellize (map convert (get-range)))
