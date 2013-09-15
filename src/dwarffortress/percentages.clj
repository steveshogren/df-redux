(ns dwarffortress.percentages)

(defn chance [x]
  (> x (rand-int 100)))

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num] 
            (let [macro-name (symbol (str "if" num))]
              `(defmacro ~macro-name [x# y#]
                 `(if (> ~~num (rand-int 100)) ~x# ~y#))))
          (range 100))))

;;(sum-up-pair-percents '((50 (+ 1 1)) (50 :f))) 
(defn sum-up-pair-percents [pairs]
  "Macro helper that sums up the pairs of percents and results,
   adding the total to each percent"
  (loop [sum 0
         ipairs pairs
         opairs []]
    (let [[num val] (first ipairs)
          next-sum (+ num sum)
          updated-pair (cons [next-sum val] opairs)]
      (if (empty? (rest ipairs))
        (reverse updated-pair)
        (recur next-sum
               (rest ipairs)
               updated-pair)))))

(defn if-percent-fn [& n]
  (if (= 1 (mod (count n) 2))
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
                (if (> current-sum 100)
                  (throw (Exception. (str "Something didn't add up")))
                  (recur current-sum (rest items)))))))
        (throw (Exception. (str "Nums: " sum " didn't equal 100" )))))))

(defn wrap-in-fn [x]
  (fn [] x))

(defn pair-flatten [col]
  "(pair-flatten [[1 [2]] [3 4] [5 6]]) -> [1 [2] 3 4 5 6]"
  (reduce (fn [items [x y]] (conj items x y))
          []
          col))

(defmacro if-percent [& n]
  "(if-percent 50 :a 49 :b 1 :c) returns :a 50% of the time, :b 49%, and :c 1%"
  (if (= 0 (mod (count n) 2))
    (let [pairs (partition 2 n)
          pairs (mapcat (fn [x] [(first x) (fn [] (second x))]) pairs)]
      `(if-percent-fn ~@pairs))))

(pprint (macroexpand '(if-percent 50 (+ 1 1) 50 :b)))
(if-percent 50 (+ 1 1) 50 (+ 2 2))

;; Transliteration of PG's aif
;; Anaphoric macro that binds to "it" the test result
(defmacro aif [test then else]
  `(let [~'it ~test]
     (if ~'it ~then ~else)))

(aif (= it 2)
     (do (print (str "it: " it)) it)
     :false)

;; Transliteration of PG's alambda
(defmacro alambda [parms & body]
  `(letfn [(~'self ~parms ~@body)]
     ~'self))
((alambda [a b] (+ a b)) 1 2)

((alambda [n]
       (if (> n 0)
         (cons
          n
          (self (- n 1))))) 5)

