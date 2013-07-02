(ns dwarffortress.dwarf-test
  (:use clojure.test
        dwarffortress.dwarf))

(deftest next-cell-toward-test
  (testing "next-cell-toward method"
    (is (= [2 2] (next-cell-toward {:x 1 :y 1} {:x 3 :y 3})))
    (is (= [2 2] (next-cell-toward {:x 1 :y 1} {:x 8 :y 8})))
    (is (= [3 2] (next-cell-toward {:x 1 :y 1} {:x 8 :y 8}
                                   [{:x 1 :y 5}
                                    {:x 2 :y 5}
                                    {:x 3 :y 5}
                                    {:x 4 :y 5}
                                    {:x 5 :y 5}
                                    {:x 5 :y 4}
                                    {:x 5 :y 3}])))
    (is (= [2 8] (next-cell-toward {:x 1 :y 8} {:x 8 :y 8})))))
