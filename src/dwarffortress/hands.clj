(ns dwarffortress.hands)

(+ 1 1)

(defn identify [h]
  (let [suit (keyword (first (map str (take 1 h))))
        num  (reduce str (drop 1 h))]
    [suit num]))

(identify "H12")

