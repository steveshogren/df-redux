(ns dwarffortress.core
  (:use [clojure.test]
        [dwarffortress.ui :only (get-world)]
        [dwarffortress.ui :only (get-world)]
        [dwarffortress.dwarf]
        [dwarffortress.map]
        [dwarffortress.percentages]
        [seesaw.core])
  (:require [dwarffortress.percentages :as p])
  (:gen-class))
;;(use 'seesaw.core)
;;(use 'seesaw.dev)
;;(show-options (grid-panel))  


(defn -main
  [& args]
  (loop [map (make-map)
         dwarf (make-dwarf)
         level 6
         uimap (get-world map dwarf level)]
    (invoke-now
     (-> (frame :title "heelo",
                :content (grid-panel :items uimap)
                :minimum-size [400 :by 400]
                :on-close :exit)
         pack!
         show!))
    #_(let [inp (read-line)]
      (recur map (update-dwarf dwarf map) 
             (case inp
               "p" (if (= 9 level) level (+ 1 level))
               "n" (if (= 0 level) level (- 1 level))
               level)))))

