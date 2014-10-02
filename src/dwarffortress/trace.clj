(ns dwarffortress.trace)
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
