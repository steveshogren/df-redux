(ns dwarffortress.ui)

(defn draw-ground [c]
  (case c
    :empty " "
    :granite "X"))

(defn draw-dwarf [d]
  (cond
   (< 3 (:hungry d)) "H"
   (< 3 (:tired d)) "T"
   :else "D"))

(defn add-cell-drawing [acc d x y level]
  (str acc d (if (= y 9) "\n") (if (= x y 9) (str "\n" level "\n"))))

(defn cell-match? [{x1 :x y1 :y z1 :z} {x2 :x y2 :y z2 :z}]
  (and (= x1 x2) (= y1 y2) (= z1 z2)))

(defn draw-world [w dwarf level]
  (println
   (reduce (fn [acc cell]
             (let [{x :x y :y z :z v :val} cell]
               (if (= z level)
                 (add-cell-drawing
                  acc  
                  (if (cell-match? cell dwarf)
                    (draw-dwarf dwarf)
                    (draw-ground v))
                  x
                  y
                  level)
                 acc)))
           "" 
           w)))


