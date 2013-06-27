(ns dwarffortress.map
  (:use [dwarffortress.percentages]))

(defn make-map []
  (take 100
        (for [z (range 10) x (range 10) y (range 10)]
          {:x x :y y :z z
           :val (if (< z 6) :wall (if-percent 90 :empty 10 :wall))})))


