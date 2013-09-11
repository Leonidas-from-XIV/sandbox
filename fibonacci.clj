(defn fibonacci [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; a memoization function. essentially the same as clojure's memoize
(defn memorize [f]
  (let [cache (ref {})]
    (fn [n]
      (if (contains? @cache n) (@cache n)
        (let [res (apply f [n])]
          (dosync
            (ref-set cache (conj {n res} @cache)))
          ;(println @cache)
          res)))))

;; a version of fibonacci with memoization - done manually
(defn memofib [n]
  (let [cache (ref {})]
    (letfn [(memorized [m]
              (if (contains? @cache m) (@cache m)
                (let [res (cond
                            (= m 0) 0
                            (= m 1) 1
                            :else (+ (memorized (- m 2)) (memorized (- m 1))))]
                  (dosync
                    (alter cache #(assoc % m res)))
                  ;(println @cache)
                  res)))]
      (memorized n))))

(defmacro defmemo [name args & body]
  `(defn ~name ~args
    ; the call cache which stores results
    (let [cache# (ref {})]
      ; yes, this shadows the outer ~name definition, but it's fine
      (letfn [(~name ~args
                (if-let [[k# v#] (find @cache# ~args)] v#
                  (let [res# ~@body]
                    (dosync
                      (alter cache# #(assoc % ~args res#)))
                    (println @cache#)
                    res#)))]
        (~name ~@args)))))


(defmemo macrofib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (macrofib (- n 1)) (macrofib (- n 2)))))

(println (fibonacci 12))
(println ((memoize fibonacci) 12))
(println ((memorize fibonacci) 12))
(println (memofib 12))
(println (macrofib 12))

(defn ackermann [m n]
  (cond (zero? m) (inc n)
        (zero? n) (ackermann (dec m) 1)
        :else (ackermann (dec m) (ackermann m (dec n)))))

(defmemo macroack [m n]
  (cond (zero? m) (inc n)
        (zero? n) (macroack (dec m) 1)
        :else (macroack (dec m) (macroack m (dec n)))))

(println (ackermann 3 4))
(println (macroack 3 4))
