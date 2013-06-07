(ns dwarffortress.percentages)

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num#] 
            (let [macro-name# (symbol (str "if" num#))]
              `(defmacro ~macro-name# [x# y#]
                 `(if (> ~~num# (rand-int 100)) ~x# ~y#))))
          (range 100))))
(make-percents)

(defmacro if-percent [& n]
  `(let [x# (list ~@n)]
    (if (= 1 (mod (count x#) 2))
      :fail
      (let [roll# (rand-int 100) 
            pairs# (partition 2 x#)]
        (if (= 100 (reduce + (filter pos? (map first pairs#))))
          (loop [ipairs# pairs#
                 total# 0]
            (let [[percent# ret#] (first ipairs#)
                  next-tot# (+ total# percent#)]
              (if (<= roll# next-tot#)
                ret#
                (recur (rest ipairs#) next-tot#))))
          :fail)))))

(= :c (with-redefs [rand-int (fn [x] 100)]
        (if-percent 50 (print "fist") 49 (print "second") 1 :c)))

(= :a (with-redefs [rand-int (fn [x] 50)]
        (if-percent 50 :a 49 :b 1 :c)))


