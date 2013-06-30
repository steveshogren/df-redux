(ns dwarffortress.dwarf
  (:use [dwarffortress.percentages]))

(defn moveIfGreater [from to]
  (cond
   (= from to) from
   (< from to) (+ from 1)
   (> from to) (- from 1)))

(defn next-cell-toward [{x :x y :y} {toX :x toY :y}]
  [(moveIfGreater x toX)
   (moveIfGreater y toY)])

(next-cell-toward {:x 1 :y 1} {:x 3 :y 3}) ; [2 2]
(next-cell-toward {:x 1 :y 1} {:x 8 :y 8}) ; [2 2]
(next-cell-toward {:x 1 :y 8} {:x 8 :y 8}) ; [2 8]

(defn make-dwarf []
  {:id (gensym) :tired 1 :hungry 1 :weapon :sword :health 1 :x 1 :y 1 :z 6})

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
                (assoc acc k (let [new-pos (if-percent 50 (inc v) 50 (dec v))]
                               (if (and (chance 50)
                                        (on-board new-pos))
                                 new-pos
                                 v)))
                :else (assoc acc k v)))
             {}
             d))

