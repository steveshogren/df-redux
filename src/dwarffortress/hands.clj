(ns dwarffortress.hands)

(def suit-ranks {:H 1 :S 2 :C 3 :D 4})

(defn identify [h]
  (let [suit (keyword (first (map str (take 1 h))))
        num (read-string (str (reduce str (drop 1 h))))]
    [suit num]))

(defn is-x-of-a-kind [cs x]
  (let [nums (map second cs)
        grouped (group-by (fn [x] x) nums)
        grouped-counts (map (fn [[num list]] [(count list) num]) grouped)
        twos (filter (fn [[count _]] (= x count)) grouped-counts)]
    (if (empty? twos)
      []
      twos)))

(defn is-two-of-a-kind [cs] (= 1 (count (is-x-of-a-kind cs 2))))
(defn is-three-of-a-kind [cs] (= 1 (count (is-x-of-a-kind cs 3)))) 
(defn is-four-of-a-kind [cs] (= 1 (count (is-x-of-a-kind cs 4))))

(defn is-two-pair [cs]
  (= 2 (count (is-x-of-a-kind cs 2))))

(defn is-full-house [cs]
  (and (is-two-of-a-kind cs) 
       (is-three-of-a-kind cs)))

(defn is-flush [cs]
  (apply = (map first cs)))

(defn is-straight [cs]
  (tracelet [nums (map second cs)
             sorted (sort nums)
             start-num (first sorted)
             expected-straight (range start-num (+ 5 start-num))
             match? (reduce (fn [x y] (and (= true x)
                                           (= x y)))
                            (map = sorted expected-straight))]
            (if match?
              sorted
              false)))

(defn rank [cs]
  (let [cs (map identify cs)]
    (cond 
          (is-flush cs) :flush
          (is-two-pair cs) :two-pair
          (is-straight cs) :straight
          (is-full-house cs) :full-house
          (is-two-of-a-kind cs) :two-kind
          (is-three-of-a-kind cs) :three-kind
          (is-four-of-a-kind cs) :four-kind
          :else :highcard)))

(= 
 (= [:H 12] (identify "H12"))
 (= :two-kind (rank ["H2" "D2" "S4" "C5" "C8"]))
 (= :highcard (rank ["H7" "D2" "S4" "C5" "C8"]))
 (= :three-kind (rank ["H7" "D7" "S7" "C5" "C8"]))
 (= :two-pair (rank ["H7" "D7" "S5" "C5" "C8"])) 
 (= :full-house (rank ["H7" "D7" "S7" "C5" "C5"])) 
 (= :flush (rank ["H7" "H3" "H9" "H5" "H2"])) 
 (= :straight (rank ["H3" "D7" "S5" "C6" "C4"])) 
 (= :four-kind (rank ["H7" "D7" "S7" "C7" "C8"])))



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
