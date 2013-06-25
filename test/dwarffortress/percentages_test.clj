(ns dwarffortress.percentages-test
  (:use clojure.test
        dwarffortress.percentages))

(deftest if-percent-test
  (testing "if-percent method"
    (is (= :b (with-redefs [rand-int (fn [x] 75)]
                (if-percent 50 :a 25 :b 25 :c))))
    (is (thrown? Exception (with-redefs [rand-int (fn [x] 51)]
                                 (if-percent 50 :a 50 :b 50 :c))))
    (is (= 4 (with-redefs [rand-int (fn [x] 51)]
               (if-percent 50 2 50 4))))
    (is (= 2 (with-redefs [rand-int (fn [x] 5)]
               (if-percent 50 2 50 (throw (Exception. "SHOULDNT CALL"))))))))

