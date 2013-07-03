(ns dwarffortress.core
  (:use [clojure.test]
        [dwarffortress.ui :only (draw-world)]
        [dwarffortress.dwarf]
        [dwarffortress.map]
        [dwarffortress.percentages]
        #_[seesaw.core])
  (:gen-class))

(defn -main
  [& args]
  #_(invoke-later
   (-> (frame :title "heelo",
              :content "hello seesaw",
              :on-close :exit)
       pack!
       show!))
  (loop [map (make-map)
         dwarf (make-dwarf)
         level 6]
    (draw-world map dwarf level)
    (let [inp (read-line)]
      (recur map (update-dwarf dwarf) 
             (case inp
               "p" (if (= 9 level) level (+ 1 level))
               "n" (if (= 0 level) level (- 1 level))
               level)))))

