(ns dwarffortress.dwarf-test
  (:use clojure.test
        dwarffortress.dwarf))

(deftest next-cell-toward-test
  (testing "next-cell-toward method"
    (is (= [2 2] (next-cell-toward {:x 1 :y 1} {:x 3 :y 3})))
    (is (= [2 2] (next-cell-toward {:x 1 :y 1} {:x 8 :y 8})))
    #_(is (= [3 2] (next-cell-toward {:x 1 :y 1} {:x 8 :y 8}
                                   [{:x 1 :y 5}
                                    {:x 2 :y 5}
                                    {:x 3 :y 5}
                                    {:x 4 :y 5}
                                    {:x 5 :y 5}
                                    {:x 5 :y 4}
                                    {:x 5 :y 3}])))
    (is (= [2 8] (next-cell-toward {:x 1 :y 8} {:x 8 :y 8})))))

(deftest should-fall?-test
  (testing "should-fall? tests"
    (is (= true (should-fall? {:x 1 :y 1 :z 5} [{:x 1 :y 1 :z 4 :val :empty}])))
    (is (= false (should-fall? {:x 1 :y 1 :z 5} [{:x 1 :y 1 :z 4 :val :wall}])))))

