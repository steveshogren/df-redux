(ns dwarffortress.core
  (:use clojure.test)
  (:gen-class))

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num#] 
            (let [macro-name# (symbol (str "if" num#))]
              `(defmacro ~macro-name# [x# y#]
                 `(if (> ~~num# (rand-int 100)) ~x# ~y#))))
          (range 100))))
(make-percents)

(defn doer [& x]
  (if (= 1 (mod (count x) 2))
    :fail
    (let [roll (rand-int 100) 
          pairs (partition 2 x)]
      (if (= 100 (reduce + (filter pos? (map first pairs))))
        (loop [ipairs pairs
               total 0]
          (let [[percent ret] (first ipairs)
                next-tot (+ total percent)]
            (if (< roll next-tot)
              ret
              (recur (rest ipairs) next-tot))))
        :fail))))

(= 2 (with-redefs [rand-int (fn [x] 5)]
       (doer 50 2 50 4)))
(= 4 (with-redefs [rand-int (fn [x] 51)]
       (doer 50 2 50 4)))
(= :fail (with-redefs [rand-int (fn [x] 51)]
       (doer 50 :a 50 :b 50 :c)))
(= :c (with-redefs [rand-int (fn [x] 75)]
       (doer 50 :a 25 :b 25 :c)))

(doer 'a 2) ;:fail
(doer 1 2 3) ;:fail
(doer 1 2 3 5)

;; (reduce-kv (fn [acc k v] (str acc k v)) "" {:1 'a})
;; (reduce-kv (fn [acc k v] (str acc k v)) "" '[a b])

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
                (assoc acc k (let [new-pos (if50 (inc v) (dec v))]
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
          [x y (if90 :empty :wall)])))

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
