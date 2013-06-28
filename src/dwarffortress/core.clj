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
         dwarf (make-dwarf)
         level 0]
    (draw-world map dwarf level)
    (let [inp (read-line)]
      (recur map (update-dwarf dwarf) 
             (case inp
               "k" (if (= 9 level) level (+ 1 level))
               "j" (if (= 0 level) level (- 1 level))
               "" level)))))

