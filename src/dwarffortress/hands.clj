(ns dwarffortress.hands
  (:use clojure.test))

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

(def suit-idents {:H 1 :S 2 :C 3 "D" 4})

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
  (let [sorted (sort nums)
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
      (let [low-ace (i-is-straight nums)
                 high-ace (i-is-straight (conj (filter #(not= 1 %) nums) 14))]
                (cond low-ace low-ace
                      high-ace high-ace
                      :else false))
      (i-is-straight nums))))


(defn is-straight-flush [cs]
  (and (is-straight cs)
       (is-flush cs)))

(defn ident [cs]
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

(def handranks {:straight-flush 1 :four-kind 2 :full-house 3 :flush 4
                :straight 5 :three-kind 6 :two-pair 7 :two-kind 8 :highcard 9})

(defn winner [h1 h2]
  (tracelet [r1 (h1 handranks)
             r2 (h2 handranks)]
            (if (> r1 r2)
              h2 h1)))

;; group-by, map, reduce, cond, if, range, sort, first, second, =, keyword, read-string, and
(testing "Hand stuff" 
  (is (= [:H 12] (identify "H12")))
  (is (= [:D 2] (identify "D2")))

  (is (= :two-kind (ident ["H2" "D2" "S4" "C5" "C8"])))
  (is (= :highcard (ident ["H7" "D2" "S4" "C5" "C8"])))
  (is (= :three-kind (ident ["H7" "D7" "S7" "C5" "C8"])))
  (is (= :two-pair (ident ["H7" "D7" "S5" "C5" "C8"]))) 
  (is (= :full-house (ident ["H7" "D7" "S7" "C5" "C5"]))) 
  (is (= :flush (ident ["H7" "H3" "H9" "H5" "H2"]))) 
  (is (= :straight (ident ["H3" "D7" "S5" "C6" "C4"]))) 
  (is (= :straight (ident ["H1" "D10" "S11" "C12" "C13"]))) 
  (is (= :straight-flush (ident ["H3" "H7" "H5" "H6" "H4"]))) 
  (is (= :four-kind (ident ["H7" "D7" "S7" "C7" "C8"])))

  (is (let [twokind (ident ["H2" "D2" "S4" "C5" "C8"])
            fourkind (ident ["H7" "D7" "S7" "C7" "C8"])]
        (= fourkind (winner twokind fourkind))))
  (is (let [twokind (ident ["H2" "D2" "S4" "C5" "C8"])
            fourkind (ident ["H7" "D7" "S7" "C7" "C8"])]
        (= fourkind (winner fourkind twokind))))
 )

