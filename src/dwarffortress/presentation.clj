(ns dwarffortress.presentation)

;; Think about templates for a second...
<html>
  <p>
  <?php> if(today_day() == "Monday") { echo("Mon")} <?/php>
  </p>
</html>

<html>
  <p>
  <% if todayDay() eq "Monday" then return "Mon" end %>
  </p>
</html>

;;They let you "disable/enable" the evaulation of code

;; no quote                +   => #<core$_PLUS_ clojure.core$_PLUS_@d3d424c>
;; ' => quote             '+   => +

;; ` => syntax quote      `+   => clojure.core/+
;; ~ => unquote           `~+  => #<core$_PLUS_ clojure.core$_PLUS_@d3d424c>
;; ~@ => unquote splicing
;;    `(~@[1 3] 1) => (1 3 1)
;;    `(~@[+ 3] 1) => (#<core$_PLUS_ clojure.core$_PLUS_@d3d424c> 3 1)

;; Function
(defn add [x y]
  (+ x y))
;; (add (+ 1 1) 3) => 5

;; "Replaced"
(defn add [2 3]
  (+ 2 3))


;; Macro 
(defmacro add [x y]
  `(+ ~x ~y))
;; (add (+ 1 1) 3) => 5

;; After "replacing" the values
(defmacro add [(+ 1 1) 3]
  `(+ (+ 1 1) 3))
;; (macroexpand '(add (+ 1 1) 3)) => (clojure.core/+ (+ 1 1) 3)


;; Macros allow for the expansion of code -without- the
;; increase in stack depth and associated attributes

;; ---- Game probability macros ----

;; revert to just chance after figuring out what is wrong with '50% and '50x
(defn chance-fn [x]
  (> x (rand-int 100)))

(defmacro chance [x]
  (let [y (Integer/parseInt (clojure.string/replace (str x) "%" ""))]
    `(chance-fn ~y)))
;; (chance %50) => (chance-fn 50)

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num] 
            (let [macro-name (symbol (str "if" num))]
              `(defmacro ~macro-name [x# y#]
                 `(if (> ~~num (rand-int 100)) ~x# ~y#))))
          (range 100))))
;; (make-percents) => (defmacro if0 ...) (defmacro if1 ...) ...
;; (pprint (macroexpand '(if1 :x :y)))









;; But this function is lame!



;; Think about what you want it to look like _first_

(if-chance "50%" :a "45%" :b "5%" :c)

(if-percent 50 :a, 45 :b, 5 :c)

(if-percent [50 :a] [45 :b] [5 :c])

(return-a [50 chance-of :a]
          [45 chance-of :b]
          [5 chance-of :c])

(if-hist ********** :a
         ********* :b
         * :c)

(defmacro if-hist  [& n]
  (if (even? (count n))
    (let [pairs (partition 2 n)
          pairs (mapcat (fn [[pct act]]
                          [(-> pct str count (* 5)) `(fn [] ~act)])
                        pairs)]
      `(if-percent-fn ~@pairs))))

(defn if-percent-fn [& n]
  "(if-percent-fn 50 (fn [] :a) 49 (fn [] :b) 1 (fn [] :c)) returns :a 50% of the time, :b 49%, and :c 1%"
  (if (odd? (count n))
    (throw (Exception. "Must pass even num of args"))
    (let [pairs (partition 2 n)
          sum (reduce + (filter pos? (map first pairs)))]
      (if (= 100 sum)
        (let [roll (inc (rand-int 100))]
          (loop [current-sum 0
                 items pairs]
            (let [[next-percent next-val] (first items)
                  current-sum (+ current-sum next-percent)]
              (if (<= roll current-sum)
                (next-val)
                (recur current-sum (rest items))))))
        (throw (Exception. (str "Nums: " sum " didn't equal 100" )))))))

(defmacro if-percent [& n]
  "(if-percent 50 :a 49 :b 1 :c) returns :a 50% of the time, :b 49%, and :c 1%"
  (if (even? (count n))
    (let [pairs (partition 2 n)
          pairs (mapcat (fn [[pct act]] [pct `(fn [] ~act)]) pairs)]
      `(if-percent-fn ~@pairs))))

#_(float (average (map (fn [x] (if-percent 50 0 50 100))
                 (range 1000))))

(defn average [col]
  (let [sum (reduce + col)
        count (count col)]
    (/ sum count)))


;; ---- Anaphoric Macros ----
(defmacro add-weird? [x]
  `(let [~'z 100]
     ~x))
(let [z 1]
  (add-weird? (+ 1 z))) ;; => 101 ...not 2 

;; Anaphoric macros allow the deliberate shadowing of
;; values when expanded. While dangerous, this can be
;; very useful in certain circumstances


;; _> is meant to "enhance" the value of the normal ->
;; command by allowing a Scala-like syntax
(defmacro _> [init & body]
  "Used to anaphorically replace _ with the result of the previous expr
   (_> 1 (+ 4 _) (+ _ 2) (* _ _)) => 49 
   (_> (+ 1 1) (* _ _)) => 4" 
  `(let [~'_ ~init
         ~@(mapcat (fn [x] `[~'_ ~x])
                   body)]
     ~'_))


;; Transliteration of PG's aif
;; Anaphoric macro that binds to "it" the test result
(defmacro aif [test then else]
  `(let [~'it ~test]
     (if ~'it ~then ~else)))

#_(aif 2
     (do (print (str "it: " it)) it)
     :false)
;;=> 2

;; Transliteration of PG's alambda
;; Anaphoric macro that binds to "self" the function for recursion
;; in typical Clojure, this should be achieved with loop/recur
(defmacro alambda [parms & body]
  `(letfn [(~'self ~parms ~@body)]
     ~'self))
#_((alambda [a b] (+ a b)) 1 2)

#_((alambda [n]
     (if (> n 0)
       (cons
        n
        (self (- n 1))))) 5)




