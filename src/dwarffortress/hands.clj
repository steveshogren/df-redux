(ns dwarffortress.hands)

(def suit-ranks {:H 1 :S 2 :C 3 :D 4})

(defn identify [h]
  (let [suit (keyword (first (map str (take 1 h))))
        num  (reduce str (drop 1 h))]
    [suit num]))

(defn is-two-of-a-kind [cs]
  (let [nums (map second cs)]
    (group-by [1 2 4 5])))

(is-two-of-a-kind h)

(defn rank [cs]
  (let [cs (map identify cs)]
    (cond (is-two-of-a-kind cs) :two-kind
          :else :nada)))
(rank h)
(def h  ["H2" "D2" "S4" "C5" "C8"])
(identify "H12")

