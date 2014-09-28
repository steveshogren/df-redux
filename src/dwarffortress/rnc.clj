(ns dwarffortress.rnc)

(defmacro trace [n f]
  `(do
     (println ~n ":" ~f)
     ~f))

(defn wrap-args-with-trace [[symb val]]
  [symb (list `trace (str "let-" symb) val)])

(defmacro tracelet [args & body]
  (let [arg-pairs (partition 2 args)
        new-bindings (vec (mapcat wrap-args-with-trace arg-pairs))]
    `(let ~new-bindings ~@body)))

(defn parse [s]
  (cond (= "I" s) 1
        (= "V" s) 5
        (= "X" s) 10
        :else 0))

