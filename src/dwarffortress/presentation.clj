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

(fib3 10)

(defn fib3 [n]
  (->> (iterate (fn [[x y]] [y (+ x y)]) [1 2])
       (take n)
       (map first)))


;; C-h r - emacs manual --> best emacs book
;; C-h i - top level help --> "elisp intro" best book on elisp
;; C-h m - modes enabled
;; => [1 2 3 5 8 13 21 34 55 89]


(defprotocol Player
  (choose [p])
  (update-strategy [p me you]))

(defrecord Stubborn [choice]
  Player
  (choose [_] choice)
  (update-strategy [this _ _] this))

(defrecord Mean [last-winner]
  Player
  (choose [_]
    (if last-winner
      last-winner
      (random-choice)))
  (update-strategy [_ me you]
    (Mean. (when (iwon? me you) me))))


