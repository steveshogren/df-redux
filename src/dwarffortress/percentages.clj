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

(defmacro if-percent [& n#]
  "(if-percent 50 :a 49 :b 1 :c) returns :a 50% of the time, :b 49%, and :c 1%"
  (if (= 1 (mod (count n#) 2))
     (throw (Exception. "Must pass even num of args"))
     (let [pairs# (partition 2 n#)
           roll (gensym "roll")
           sum# (reduce + (filter pos? (map first pairs#)))]
       `(if (= 100 ~sum#)
         (let [~roll (rand-int 100)]
            (cond
             ~@(mapcat (fn [[per# val#]]
                       (list `(<= ~roll ~per#) val#))
                     (sum-up-pair-percents pairs#))))
         (throw (Exception. (str "Nums: " ~sum# " didn't equal 100" )))))))

