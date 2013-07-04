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

(defn find-cell [{cellx :x celly :y cellz :z} map]
  (first (filter (fn [{x :x y :y z :z}]
                   (and (= x cellx) (= y celly) (= z cellz)))
                 map)))

(defn should-fall? [{x :x y :y z :z} map]
  (-> (find-cell {:x x :y y :z (- z 1)} map)
    :val
    (= :empty)))

(defn make-dwarf []
  {:id (gensym) :tired 1 :hungry 1 :weapon :sword :health 1 :x 1 :y 1 :z 8})

(defn should-increase-need [need]
  (and (> 10 need)
       (chance 10)))

(defn update-need [need]
  (if (should-increase-need need)
    (inc need)
    need))

(defn on-board [x]
  (and (<= x 10) (>= x 0)))

(defn move-rand [v d map]
  (if (not (should-fall? d map))
    (let [new-pos (if-percent 50 (inc v) 50 (dec v))]
      (if (and (chance 50)
               (on-board new-pos))
        new-pos
        v))
    v))

(defn apply-gravity [d map]
  (if (should-fall? d map)
    (-> d :z (- 1))
    (:z d)))

(defn update-dwarf [d map]
  (reduce-kv (fn [acc k v]
               (cond
                (or (= k :tired) (= k :hungry)) (assoc acc k (update-need v))
                (or (= :x k) (= :y k)) (assoc acc k (move-rand v d map))
                (= :z k) (assoc acc k (apply-gravity d map))
                :else (assoc acc k v)))
             {}
             d))

