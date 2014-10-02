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

(defn parse-single [s]
  (get nums s))

(defn parse-doubles [s]
  (tracelet [pairs (partition 2 1 (map parse-single (seq s)))
        [f s] (first pairs)
        first-val (if (> f s) f 0)
        rest-vals (map (fn [[f s]] (if (< f s) (- s f) s)) pairs)]
    (reduce + first-val rest-vals)))

(defn parse [s]
  (if (= 1 (count s))
    (reduce + (map parse-single (seq s)))
    (parse-doubles s)))

;; (parse "")


