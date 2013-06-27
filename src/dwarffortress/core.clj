(ns dwarffortress.core
  (:use [clojure.test]
        [dwarffortress.ui :only (draw-world)]
        [dwarffortress.dwarf]
        [dwarffortress.map]
        [dwarffortress.percentages])
  (:gen-class))

(defn -main
  [& args]
  (loop [map (make-map)
         dwarf (make-dwarf)]
    (draw-world map dwarf)
    (let [inp (read-line)]
      (if (= "" inp)
        (recur map (update-dwarf dwarf))))))
