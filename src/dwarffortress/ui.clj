(ns dwarffortress.ui
  (:use [dwarffortress.map]))

(defn draw-ground [c]
  (case c
    :empty " "
    :granite "X"))

(defn draw-dwarf [d]
  (cond
   (< 3 (:hungry d)) "H"
   (< 3 (:tired d)) "T"
   :else "D"))

(defn cell-match? [{x1 :x y1 :y z1 :z} {x2 :x y2 :y z2 :z}]
  (and (= x1 x2) (= y1 y2) (= z1 z2)))

(defn this-level? [{z1 :z} z2] (= z1 z2))

(defn get-world [w dwarf level]
  (partition 10
             (map (fn [cell]
                    (if (cell-match? cell dwarf)
                      (draw-dwarf dwarf)
                      (draw-ground v)))
                  (filter #(this-level? %1 level) w))))

(get-world (make-map) {:x 1 :y 2 :z 1} 1)

(defn draw-world [w dwarf level]
  (map println (get-world w dwarf level)))


