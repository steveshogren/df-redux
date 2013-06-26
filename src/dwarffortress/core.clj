(ns dwarffortress.core
  (:use [clojure.test]
        [dwarffortress.ui :only (draw-world)]
        [dwarffortress.dwarf]
        [dwarffortress.percentages])
  (:gen-class))

(defn make-dwarf []
  {:id (gensym) :tired 1 :hungry 1 :weapon :sword :health 1 :x 1 :y 1})

(defn make-map []
  (take 100
        (for [x (range 10) y (range 10)]
          [x y (if-percent 90 :empty 10 :wall)])))

(defn -main
  [& args]
  (loop [map (make-map)
         dwarf (make-dwarf)]
    (draw-world map dwarf)
    (let [inp (read-line)]
      (if (= "" inp)
        (recur map (update-dwarf dwarf))))))
