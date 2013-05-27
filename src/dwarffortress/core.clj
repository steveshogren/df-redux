(ns dwarffortress.core
  (:gen-class))

;;(p25 x y)
(defmacro make-percents []
  (let [macro-name (symbol (str "p" 25))
        x (gensym "x__")
        y (gensym "y__")]
    `(defmacro ~macro-name [~x ~y]
       (if (> 25 (rand-int 100)) ~x ~y))))

(macroexpand '(make-percents))
(make-percents)
(p25 (print "x") (print "y"))

;; todo -> percentage macro generator: maybe (p25 x y)
(defn chance [x]
  (> x (rand-int 100)))

(defn make-dwarf []
  {:id (gensym) :tired 1 :hungry 1 :weapon :sword :health 1 :x 1 :y 1})

(defn should-increase-need [need]
  (and (> 10 need)
       (chance 10)))

(defn update-need [need]
  (if (should-increase-need need)
    (inc need)
    need))

(defn on-board [x]
  (and (<= x 10) (>= x 0)))

(defn update-dwarf [d]
  (reduce-kv (fn [acc k v]
               (cond
                (or (= k :tired) (= k :hungry))
                (assoc acc k (update-need v))
                (or (= :x k) (= :y k))
                (assoc acc k (let [new-pos (if (chance 50) (inc v) (dec v))]
                               (if (and (chance 50)
                                        (on-board new-pos))
                                 new-pos
                                 v)))
                :else (assoc acc k v)))
             {}
             d))

(defn make-map []
  (take 100
        (for [x (range 10) y (range 10)]
          [x y (if (> 0.9 (rand 1)) :empty :wall)])))

(defn draw-cell [c]
  (case c
    :empty " "
    :wall "X"))
(defn draw-dwarf [d]
  (cond
   (< 3 (:hungry d)) "H"
   (< 3 (:tired d)) "T"
   :else "D"))

(defn add-cell-drawing [acc d y]
  (str acc d (if (= y 9) "\n")))

(defn draw-world [w d]
  (println
   (reduce (fn [acc [x y v]]
             (if (and (= x (:x d)) (= y (:y d)))
               (add-cell-drawing acc (draw-dwarf d) y)
               (add-cell-drawing acc (draw-cell v) y)))
           "" 
           w)))

(defn -main
  [& args]
  (loop [map (make-map)
         dwarf (make-dwarf)]
    (draw-world map dwarf)
    (let [inp (read-line)]
      (if (= " " inp)
        (recur map (update-dwarf dwarf))))))
