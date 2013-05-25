(ns dwarffortress.core
  (:gen-class))

(defn make-dwarf []
  {:id (gensym) :tired 0 :hungry 0})
(def d (make-dwarf))

(defn should-increase-need [need]
  (< (rand-int 100) (rand-int need)))

(defn increase-need [d need]
  (assoc d need (inc (need d))))

(defn update-dwarf [dwarf]
  (loop [d dwarf
         need :tired]
    (if (should-increase-need (need d))
      (recur d :hungry)
      (increase-need d :tired))))

(loop)

(defn make-world [n] 1)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
