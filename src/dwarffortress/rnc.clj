(ns dwarffortress.rnc
  (:require [dwarffortress.trace :refer [trace tracelet]]))

(def nums {\I 1 \V 5 \X 10})
(def num-s {1 "I" 5 "V" 10 "X"})

(defn parse-single [c] (get nums c))

(defn parse-pair [[f s]] (if (< f s) (- s f) s))

(defn parse-doubles [s]
  (let [pairs (partition 2 1 (map parse-single (seq s)))
        [f s] (first pairs)
        first-val (if (>= f s) f 0)
        rest-vals (map parse-pair pairs)]
     (reduce + first-val rest-vals)))

(defn valid? [s]
  (let [quads (partition 4 1 (seq s))]
    (reduce (fn [acc next]
              ;; Ensure no 4x repeated characters
              (and (apply not= next)
                   acc))
            true
            quads)))

(defn parse [s]
  (cond (not (valid? s)) 0
        (= 1 (count s)) (parse-single (first (seq s)))
        :else (parse-doubles s)))

(defn find-smaller [i]
  (apply max (filter #(< % i) (keys num-s))))

(defn get-roman [i] (get num-s i))

(defn to-roman-multiple [i]
  (loop [remaining i
         accum ""]
    (if (= 0 remaining)
      accum
      (if-let [match (get-roman remaining)]
        (str accum match)
        (let [smaller (find-smaller remaining)]
          (recur (- remaining smaller)
                 (str accum (get-roman smaller))))))))

(defn to-roman [i]
  (let [single (get-roman i)]
    (if single single
        (to-roman-multiple i))))

