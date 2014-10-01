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

(def nums {\I 1 \V 5 \X 10})

(defn parse-doubles [s]
  (let [pairs (partition 2 (map parse-single s))]
    (map (fn [[f s]] (if (> f s) (- s f) s)) pairs)
    ))

(defn parse-single [s]
  (get nums s))

(defn parse [s]
  (reduce + (map parse-single (seq s))))

