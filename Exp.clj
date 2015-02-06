(deftype Const [value])
(deftype Sum [left right])
(deftype Prod [left right])

(defmulti evaluate type)

(defmethod evaluate Const [c] (.value c))
(defmethod evaluate Sum [s] (+ (evaluate (.left s)) (evaluate (.right s))))
(defmethod evaluate Prod [p] (* (evaluate (.left p)) (evaluate (.right p))))

(def e (Sum. (Const. 1)
            (Prod. (Sum. (Const. 2)
                       (Const. 5))
                  (Const. 3))))

(println (evaluate e))
