(ns dwarffortress.hands)

(def suit-ranks {:H 1 :S 2 :C 3 :D 4})

(defn identify [h]
  (let [suit (keyword (first (map str (take 1 h))))
        num  (reduce str (drop 1 h))]
    [suit num]))

(defn is-two-of-a-kind [cs]
  (let [nums (map second cs)
        grouped (group-by (fn [x] x) nums)
        grouped-counts (map (fn [[num list]] [(count list) num]) grouped)
        twos (filter (fn [[count _]] (= 2 count)) grouped-counts)]
    (if (empty? twos)
      false
      twos)))
(defn is-three-of-a-kind [cs]
  (let [nums (map second cs)
        grouped (group-by (fn [x] x) nums)
        grouped-counts (map (fn [[num list]] [(count list) num]) grouped)
        twos (filter (fn [[count _]] (= 3 count)) grouped-counts)]
    (if (empty? twos)
      false
      twos)))

(defn is-four-of-a-kind [cs]
  (let [nums (map second cs)
        grouped (group-by (fn [x] x) nums)
        grouped-counts (map (fn [[num list]] [(count list) num]) grouped)
        twos (filter (fn [[count _]] (= 4 count)) grouped-counts)]
    (if (empty? twos)
      false
      twos)))

(defn rank [cs]
  (let [cs (map identify cs)]
    (cond (is-two-of-a-kind cs) :two-kind
          (is-three-of-a-kind cs) :three-kind
          (is-four-of-a-kind cs) :four-kind
          :else :highcard)))

(= [:H "12"] (identify "H12"))
(= :two-kind (rank ["H2" "D2" "S4" "C5" "C8"]))
(= :highcard (rank ["H7" "D2" "S4" "C5" "C8"]))
(= :three-kind (rank ["H7" "D7" "S7" "C5" "C8"]))
(= :four-kind (rank ["H7" "D7" "S7" "C7" "C8"]))
(= :two-pair (rank ["H7" "D7" "S9" "C9" "C8"]))

(defmacro trace [n f]
  `(do
     (println ~n ":" ~f)
     ~f))

(defn wrap-args-with-trace [[symb val]]
  [symb (list `trace (str "let-" symb) val)])

(defmacro tracelet [args & body]
  (let [arg-pairs (partition 2 args)
        new-bindings (vec (mapcat wrap-args-with-trace arg-pairs))]
    `(let ~new-bindings ~@body)))
