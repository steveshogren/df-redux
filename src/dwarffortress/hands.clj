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

;; Windows         - "Default setup"
;; Cider           - "Connection stuff"
;; Evil/Paredit    - "Amazing Parens"
;; Company         - "DONE"

(def suit-idents {:H 1 :S 2 :C 3 "D" 4})

(defn identify [h]
  (let [suit (keyword (first (map str (take 1 h))))
        num (read-string (str (reduce str (drop 1 h))))]
    [suit num]))

(defn is-x-of-a-kind [cs x]
  (tracelet [nums (map second cs)
             grouped (group-by (fn [x] x) nums)
             grouped-counts (map (fn [[num list]] [(count list) num]) grouped)
             ns (filter (fn [[count _]] (= x count)) grouped-counts)]
            (if (empty? ns)
              []
              ns)))

(defn is-two-of-a-kind [cs]
  (let [pair-counts (is-x-of-a-kind cs 2)]
    {:ismatch (= 1 (count pair-counts)) :high (second pair-counts)}))

(defn is-three-of-a-kind [cs]
  (let [pair-counts (is-x-of-a-kind cs 3)]
    {:ismatch (= 1 (count pair-counts)) :high (second pair-counts)})) 

(defn is-four-of-a-kind [cs]
  (let [pair-counts (is-x-of-a-kind cs 4)]
    {:ismatch (= 1 (count pair-counts)) :high (second pair-counts)}))

(defn is-two-pair [cs]
  (let [pair-counts (is-x-of-a-kind cs 2)]
    {:ismatch (= 2 (count pair-counts)) :high (max (map second pair-counts))}))

(defn is-full-house [cs]
  (let [two (is-two-of-a-kind cs) 
        three (is-three-of-a-kind cs)]
    {:ismatch (and (:ismatch two)
                   (:ismatch three))
     :high (max (map :high [two three]))}))

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
     (is-straight-flush cs) [:straight-flush cs]
     (is-flush cs) [:flush cs]
     (is-two-pair cs) [:two-pair cs]
     (is-straight cs) [:straight cs]
     (is-full-house cs) [:full-house cs]
     (is-two-of-a-kind cs) [:two-kind cs]
     (is-three-of-a-kind cs) [:three-kind cs]
     (is-four-of-a-kind cs) [:four-kind cs]
     :else [:highcard cs])))

(def handranks {:straight-flush 1 :four-kind 2 :full-house 3 :flush 4
                :straight 5 :three-kind 6 :two-pair 7 :two-kind 8 :highcard 9})

(defn rank-same-hand [h1 h2]
  false)

(defn winner [h1 h2]
  (tracelet [r1 ((first h1) handranks)
             r2 ((first h2) handranks)]
            (cond (> r1 r2) h2
                  (< r1 r2) h1
                  :else (rank-same-hand h1 h2))))

;; group-by, map, reduce, cond, if, range, sort, first, second, =, keyword, read-string, and
(testing "Hand stuff" 
  (is (= [:H 12] (identify "H12")))
  (is (= [:D 2] (identify "D2"))))

(testing "Hand identification"
  (is (= :two-kind (first (ident ["H2" "D2" "S4" "C5" "C8"]))))
  (is (= :highcard (first (ident ["H7" "D2" "S4" "C5" "C8"]))))
  (is (= :three-kind (first (ident ["H7" "D7" "S7" "C5" "C8"]))))
  (is (= :two-pair (first (ident ["H7" "D7" "S5" "C5" "C8"])))) 
  (is (= :full-house (first (ident ["H7" "D7" "S7" "C5" "C5"])))) 
  (is (= :flush (first (ident ["H7" "H3" "H9" "H5" "H2"])))) 
  (is (= :straight (first (ident ["H3" "D7" "S5" "C6" "C4"])))) 
  (is (= :straight (first (ident ["H1" "D10" "S11" "C12" "C13"])))) 
  (is (= :straight-flush (first (ident ["H3" "H7" "H5" "H6" "H4"])))) 
  (is (= :four-kind (first (ident ["H7" "D7" "S7" "C7" "C8"])))))

(testing "Hank ranking"
  (is (let [twokind (ident ["H2" "D2" "S4" "C5" "C8"])
            fourkind (ident ["H7" "D7" "S7" "C7" "C8"])]
        (= fourkind (winner twokind fourkind))))

  (is (let [twokind (ident ["H2" "D2" "S4" "C5" "C8"])
            fourkind (ident ["H7" "D7" "S7" "C7" "C8"])]
        (= fourkind (winner fourkind twokind))))

  (is (let [h (ident ["H9" "D9" "S9" "C9" "C8"])
            l (ident ["H7" "D7" "S7" "C7" "C8"])]
        (= h (winner h l)))) 

  (is (let [l (ident ["H7" "D7" "S7" "C7" "C8"])
            h (ident ["H9" "D9" "S9" "C9" "C8"])]
        (= h (winner l h))))

  )


