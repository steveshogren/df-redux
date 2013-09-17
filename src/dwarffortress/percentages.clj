(ns dwarffortress.percentages)

(defmacro _> [init & body]
  "Used to anaphorically fill _ with the result of the previous expr
   (_> 1 (+ 4 _) (+ _ 2) (* _ _)) => 49 
   (_> (+ 1 1) (* _ _)) => 4" 
  `(let [~'_ ~init
         ~@(mapcat (fn [x] `[~'_ ~x])
                   body)]
     ~'_))

(defn chance [x]
  (> x (rand-int 100)))

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num] 
            (let [macro-name (symbol (str "if" num))]
              `(defmacro ~macro-name [x# y#]
                 `(if (> ~~num (rand-int 100)) ~x# ~y#))))
          (range 100))))

(defn if-percent-fn [& n]
  (if (odd? (count n))
    (throw (Exception. "Must pass even num of args"))
    (let [pairs (partition 2 n)
          sum (reduce + (filter pos? (map first pairs)))]
      (if (= 100 sum)
        (let [roll (rand-int 100)]
          (loop [current-sum 0
                 items pairs]
            (let [[next-percent next-val] (first items)
                  current-sum (+ current-sum next-percent)]
              (if (<= roll current-sum)
                (next-val)
                (recur current-sum (rest items))))))
        (throw (Exception. (str "Nums: " sum " didn't equal 100" )))))))

(defmacro if-percent [& n]
  "(if-percent 50 :a 49 :b 1 :c) returns :a 50% of the time, :b 49%, and :c 1%"
  (if (even? (count n))
    (let [pairs (partition 2 n)
          pairs (mapcat (fn [[pct act]] [pct `(fn [] ~act)]) pairs)]
      `(if-percent-fn ~@pairs))))

#_(float (average (map (fn [x] (if-percent 50 0 50 10))
                 (range 100000))))

(defn average [col]
  (let [sum (reduce + col)
        count (count col)]
    (/ sum count)))

;; Transliteration of PG's aif
;; Anaphoric macro that binds to "it" the test result
(defmacro aif [test then else]
  `(let [~'it ~test]
     (if ~'it ~then ~else)))

#_(aif (= it 2)
     (do (print (str "it: " it)) it)
     :false)

;; Transliteration of PG's alambda
;; Anaphoric macro that binds to "self" the function for recursion
;; in typical Clojure, this should be achieved with loop/recur
(defmacro alambda [parms & body]
  `(letfn [(~'self ~parms ~@body)]
     ~'self))
#_((alambda [a b] (+ a b)) 1 2)

#_((alambda [n]
     (if (> n 0)
       (cons
        n
        (self (- n 1))))) 5)





