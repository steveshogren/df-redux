(ns dwarffortress.core
  (:gen-class))

(defn make-dwarf []
  {:id (gensym) :tired 1 :hungry 1})

(defn should-increase-need [need]
  (and (> 10 need)
       (< (rand-int 10) 9)))

(defn update-need [need]
  (if (should-increase-need need)
    (inc need)
    need))

(def d (make-dwarf))
(defn update-dwarf [d]
  (reduce-kv (fn [acc k v]
               (if (or (= k :tired)
                       (= k :hungry))
                 (assoc acc k (update-need v))
                 (assoc acc k v)))
             {}
             d))
(loop [x 0
       d (make-dwarf)]
  (if (= x 100)
    d
    (recur (inc x) (update-dwarf d))))

(defn make-world [n] 1)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
