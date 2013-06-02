(ns dwarffortress.percentages-test
  (:use clojure.test
        dwarffortress.percentages))

(deftest if-percent-test
  (testing "if-percent method"
    (is (= :c (with-redefs [rand-int (fn [x] 75)]
                (doer 50 :a 25 :b 25 :c))))
    (is (= :fail (with-redefs [rand-int (fn [x] 51)]
                   (doer 50 :a 50 :b 50 :c))))
    (is (= 4 (with-redefs [rand-int (fn [x] 51)]
               (doer 50 2 50 4))))
    (is (= 2 (with-redefs [rand-int (fn [x] 5)]
               (doer 50 (+ 2 2) 50 (+ 2 4)))))))
;; (macroexpand '(doer 50 2 50 5))


      


;; (reduce-kv (fn [acc k v] (str acc k v)) "" {:1 'a})
;; (reduce-kv (fn [acc k v] (str acc k v)) "" '[a b])
