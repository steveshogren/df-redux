(ns dwarffortress.presentation)

(comment A fib sequence "starting" with the pair [1 2])

(defn fib [n]
  ;; Starts count at 1 to "skip" the
  ;; auto-added first digit
  (loop [count 1
         x 1
         y 2
         ret [x]]
    (if (= count n)
      ret
      (recur (inc count)
             y
             (+ x y)
             (conj ret y)))))

(fib 10)
;; => [1 2 3 5 8 13 21 34 55 89]

(defn fib2 [n]
  (map first (take n (iterate (fn [[x y]] [y (+ x y)]) [1 2]))))

(fib2 10)

;; => [1 2 3 5 8 13 21 34 55 89]
