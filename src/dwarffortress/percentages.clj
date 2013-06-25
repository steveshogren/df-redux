(ns dwarffortress.percentages)

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num#] 
            (let [macro-name# (symbol (str "if" num#))]
              `(defmacro ~macro-name# [x# y#]
                 `(if (> ~~num# (rand-int 100)) ~x# ~y#))))
          (range 100))))
(make-percents)

(defmacro if-percent [& n#]
  #_(print (partition 2 n#))
  `(if (= 1 (mod (count ~n#) 2))
     :fail ;; have to pass an even num of args
     (let [roll# (rand-int 100) 
           pairs# (partition 2 ~n#)]
       (if (= 100 (reduce + (filter pos? (map first pairs#))))
         (loop [ipairs# pairs#
                total# 0]
           (let [[percent# ret#] (first ipairs#)
                 next-tot# (+ total# percent#)]
             (if (<= roll# next-tot#)
               (print ret#)
               (recur (rest ipairs#) next-tot#))))
         :fail)))) ;; nums have to = 100

;;(sum-up-pair-percents '((50 (+ 1 1)) (50 :f))) 
(defn sum-up-pair-percents [pairs]
  "Sums up the pairs of percents and results,
   adding the total to each percent"
  (loop [sum 0
         ipairs pairs
         opairs []]
    (let [[num val] (first ipairs)
          next-sum (+ num sum)
          updated-pair [next-sum val]
          updated-opair (cons updated-pair opairs)]
      (if (empty? (rest ipairs))
        (reverse updated-opair)
        (recur next-sum
               (rest ipairs)
               updated-opair)))))

(defmacro if-percent [& n#]
  #_(print (partition 2 n#))
  (if (= 1 (mod (count n#) 2))
     :fail ;; have to pass an even num of args
     (let [pairs# (partition 2 n#)]
       (if (= 100 (reduce + (filter pos? (map first pairs#))))
         `(let [roll# (rand-int 100)
                sum-pairs# (map )]
            (cond
             (list ~@(map (fn [pair#] (list (<= roll# )))) )))
         :fail)))) ;; nums have to = 100

(defn yep [prt ret] (print prt) ret)
(let [[a b] '(1 (test))]
  b)

(= :c (with-redefs [rand-int (fn [x] 100)]
        (if-percent 50 (yep "first" :a) 49 (yep "second" :b) 1 :c)))
(pprint (macroexpand '(if-percent 50 (yep "first" :a) 49 (yep "second" :b) 1 :c)))

(= :a (with-redefs [rand-int (fn [x] 50)]
        (if-percent 50 :a 49 :b 1 :c)))


