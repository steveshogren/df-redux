(ns dwarffortress.hands)

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

(def suit-ranks {:H 1 :S 2 :C 3 "D" 4})

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

(defn i-is-straight [nums]
  (tracelet [sorted (sort nums)
             start-num (first sorted)
             expected-straight (range start-num (+ 5 start-num))
             match? (reduce (fn [x y] (and (= true x)
                                           (= x y)))
                            (map = sorted expected-straight))]
            (if match?
              sorted
              false)))

(defn is-straight [cs]
  (let [nums (map second cs)]
    (if (= 1 (count (filter #(= 1 %) nums)))
      (tracelet [low-ace (i-is-straight nums)
                 high-ace (i-is-straight (conj (filter #(not= 1 %) nums) 14))]
                (cond low-ace low-ace
                      high-ace high-ace
                      :else false))
      (i-is-straight nums))))


(defn is-straight-flush [cs]
  (and (is-straight cs)
       (is-flush cs)))

(defn rank [cs]
  (let [cs (map identify cs)]
    (cond 
          (is-straight-flush cs) :straight-flush
          (is-flush cs) :flush
          (is-two-pair cs) :two-pair
          (is-straight cs) :straight
          (is-full-house cs) :full-house
          (is-two-of-a-kind cs) :two-kind
          (is-three-of-a-kind cs) :three-kind
          (is-four-of-a-kind cs) :four-kind
          :else :highcard)))

;; group-by, map, reduce, cond, if, range, sort, first, second, =, keyword, read-string, and
(= 
 (= [:H 12] (identify "H12"))
 (= :two-kind (rank ["H2" "D2" "S4" "C5" "C8"]))
 (= :highcard (rank ["H7" "D2" "S4" "C5" "C8"]))
 (= :three-kind (rank ["H7" "D7" "S7" "C5" "C8"]))
 (= :two-pair (rank ["H7" "D7" "S5" "C5" "C8"])) 
 (= :full-house (rank ["H7" "D7" "S7" "C5" "C5"])) 
 (= :flush (rank ["H7" "H3" "H9" "H5" "H2"])) 
 (= :straight (rank ["H3" "D7" "S5" "C6" "C4"])) 
 (= :straight (rank ["H1" "D10" "S11" "C12" "C13"])) 
 (= :straight-flush (rank ["H3" "H7" "H5" "H6" "H4"])) 
 (= :four-kind (rank ["H7" "D7" "S7" "C7" "C8"])))

