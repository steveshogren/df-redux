(ns dwarffortress.percentages)

(defn chance [x]
  (> x (rand-int 100)))

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num#] 
            (let [macro-name# (symbol (str "if" num#))]
              `(defmacro ~macro-name# [x# y#]
                 `(if (> ~~num# (rand-int 100)) ~x# ~y#))))
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


(defn is-type2 [coll type]
  (reduce (fn [ret k]
            (and ret
                 (contains? coll k)))
          true
          type))

(def check-types-in-deft true)

(defn rand-string [x] (symbol (str "auto_" (rand-int 1000000))))

;; (defmacro deft [name# &args# rett# body#]
(defmacro deft [name# & rest#]
  "defnt [name doc? [param Type*] Type body]
   (defnt walk [duck Duck] Duck
     (body must return duck shape...))"
  (let [hasDoc# (string? (first rest#))
        doc# (if hasDoc# (first rest#) "")
        args# (if hasDoc# (second rest#) (first rest#))
        rett# (if hasDoc# (nth rest# 2) (second rest#))
        body# (last rest#)]
  (if (= 0 (mod (count args#) 2))
    (let [argpairs# (partition 2 args#)
          argnames# (vec (map first argpairs#))
          argtypes# (vec (map second argpairs#))
          cleanedArgs# (vec (map rand-string argnames#))
          putBackArgs# (mapcat (fn [x] x) (map vector argnames# cleanedArgs#))
          is-type# (fn [coll# type#] (reduce (fn [iret# k#]
                                           (and iret#
                                                (contains? coll# k#)))
                                         true
                                         type#))]
      `(defn ~name#
         #_{:arglists (vec (mapcat (fn [x#] x#) (map vector '~argnames# ~argtypes#)))}
        {:doc (str (vec (mapcat (fn [x#] x#) (map vector '~argnames# ~argtypes#)))" -> " ~rett# "\n  " ~doc#)}
         ~cleanedArgs#
         (if check-types-in-deft
           (if (reduce (fn [oret# pair#] 
                         (and oret# (~is-type# (first pair#) (second pair#))))
                       true
                       (map vector ~cleanedArgs# ~argtypes#)) ;; all params match type
             (let [~@putBackArgs#
                   ret# ~body#]
               (if (~is-type# ret# ~rett#)
                 ret#
                 :wrongtypereturned))
             :argsdontmatchtypes)
           (let [~@putBackArgs#]
             ~body#))))
    :missingtypesfail)))


(def Account [:id :balance])
(def Pay [:amount])
(pprint (macroexpand
'(deft adds "Adds a payment to an account" [account Account pay Pay] Account
  (assoc account :balance (+ (:amount pay) (:balance account))))))
(deft adds "Adds a payment to an account" [account Account pay Pay] Account
  (assoc account :balance (+ (:amount pay) (:balance account))))
(adds {:balance 2 :id 1} {:amount 2}) 
;; {:balance 4 :id 1}

(adds {:balance 2} {:amount 2})
;; :argsdontmatchtypes

(deft wrongRet [account Account pay Pay] Account
  (+ 1 2)) 
(wrongRet {:balance 2 :id 1} {:amount 2})
;; :wrongtypereturned 

;;Disable all type checking
(def check-types-in-defnt false)
(adds 2 2)
;; NullPointerException

;; You can disable/circumvent typeshape checking
;; by simply using an empty vec for a "type".
;; No keywords, nothing to check, empty vec is our Unit
(deft noTypes [num [] account [:balance]] []
  (+ num (:balance account)))
(noTypes 1 {:balance 1}) ;;2

;; You can still destructure parameters
(def Coord [:x :y])
(deft addX "Adds a Coord x value to Coord" [pos Coord {x :x} Coord] Coord 
  (assoc pos :x (+ x (:x pos))))

(addX {:x 1 :y 100} {:x 1 :y 100 :z 40}) ;;{:y 100 :x 2}


