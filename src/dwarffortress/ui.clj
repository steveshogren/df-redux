(ns dwarffortress.ui)

(defn draw-cell [c]
  (case c
    :empty " "
    :wall "X"))
(defn draw-dwarf [d]
  (cond
   (< 3 (:hungry d)) "H"
   (< 3 (:tired d)) "T"
   :else "D"))

(defn add-cell-drawing [acc d y]
  (str acc d (if (= y 9) "\n")))

(defn draw-world [w d]
  (println
   (reduce (fn [acc {x :x y :y z :z v :val}]
             (if (and (= x (:x d)) (= y (:y d)))
               (add-cell-drawing acc (draw-dwarf d) y)
               (add-cell-drawing acc (draw-cell v) y)))
           "" 
           w)))


