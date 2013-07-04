(ns dwarffortress.dwarf
  (:use [dwarffortress.percentages]))

(defn move-if-greater [from to]
  (cond
   (= from to) from
   (< from to) (+ from 1)
   (> from to) (- from 1)))

(defn next-cell-toward
  ;;"Give the next cell within 1 block closest"
  ([{x :x y :y} {toX :x toY :y} blockages]
     [(move-if-greater x toX) (move-if-greater y toY)])
  ([cell cell2]
     (next-cell-toward cell cell2 [])))

(defn find-cell [{cellx :x celly :y} map]
  (first (filter (fn [{x :x y :y}]
                   (and (= x cellx) (= y celly)))
                 map)))

(defn should-fall? [{x :x y :y z :z} map]
  (= :empty (:val (find-cell {:x x :y y :z (- z 1)} map))))


(should-fall? {:x 1 :y 1 :z 5} [{:x 1 :y 1 :z 4 :val :empty}])
(should-fall? {:x 1 :y 1 :z 5} [{:x 1 :y 1 :z 4 :val :wall}])

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

