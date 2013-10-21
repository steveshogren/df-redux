(ns dwarffortress.presentation)

;; Think about templates for a second...
<html>
  <p>
  <?php> if(today_day() == "Monday") { echo("Mon")} </?>
  </p>
</html>

<html>
  <p>
  <% if todayDay() eq "Monday" then return "Mon" end %>
  </p>
</html>

;;They let you "disable/enable" the evaulation of code,
;; the template is what is returned, but first the code
;; blocks are evaluated, allowing for expansion of the template

<html>
  <ul>
  <% foreach (var person in people) {  %>
     <li> <% person.name %> </li>
  <% } %>
  </ul>
</html>


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
(defn chance [x]
  (> x (rand-int 100)))


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

(if-percent-b [50 :a] [45 :b] [5 :c])

(if-percent-c :a 50 :b 45 :c 5)

(return-a 50 % chance of :a
          45 % chance of :b
          5 % chance of :c)

(if-percent ********** :a
            *********  :b
            *          :c)



            
;; So these are cool, but how do we make them work on seqs?
(defmacro adder [x y] `(+ ~x ~y))
(reduce adder 0 [4 5])
;; => java.lang.RuntimeException: Can't take value of a macro: #'user/adder


;; We make a regular function syntax that might be unwieldy, but allowing at
;; least the _ability_ to use over collections
(if-percent-fn 50 (fn [] :a)
               49 (fn [] :b)
               1 (fn [] :c))

(defn if-percent-fn [& n]
  " Takes a set of percent chances and return functions
    to call if that 'chance' happens. All percents must
    add up to 100, but any number are supported.
   (if-percent-fn 50 (fn [] :a)
                  49 (fn [] :b)
                  1 (fn [] :c))
    => returns :a 50% of the time, :b 49%, and :c 1%"
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






;; This frees us now to explore shorter or more expressive syntaxes





(defn convert-to-num [x]
  (if (number? x) x
      (-> x str count (* 5))))

(defmacro if-percent [& n]
  "(if-percent 50 :a 49 :b 1 :c) returns :a 50% of the time, :b 49%, and :c 1%
   (if-percent ********** :a
               *********  :b
               *          :c) => 50% :a, 45% :b, 5% :c "
  (if (even? (count n))
    (let [pairs (partition 2 n)
          pairs (mapcat (fn [[pct act]]
                          [(convert-to-num pct) `(fn [] ~act)])
                        pairs)]
      `(if-percent-fn ~@pairs))))


#_(->> (range 1000)
       (map (fn [x] (if-percent 50 1 50 100)))
       average 
       float)


(defn average [col]
  (let [sum (reduce + col)
        count (count col)]
    (/ sum count)))


;; ---- Anaphoric Macros ----

;; Here we shadow the z value with another
(defmacro add-weird [x]
  `(let [~'z 100]
     ~x))
(let [z 1]
  (add-weird (inc z))) ;; => 101 ...not 2 



;; Anaphoric macros allow the deliberate shadowing of
;; values when expanded. While dangerous, this can be
;; very useful in certain circumstances




;; _> is meant to "enhance" the value of the normal ->
;; command by allowing a Scala-like syntax
(defmacro _> [init & body]
  "Used to anaphorically replace _ with the result of the previous expr
   (_> 1 (+ 4 _) (+ _ 2) (* _ _)) => 49 
   (_> (+ 1 1) (* _ _)) => 4 " 
  `(let [~'_ ~init
         ~@(mapcat (fn [x] `[~'_ ~x])
                   body)]
     ~'_))


#_(_> (range 1000)
      (map (fn [x] (if-percent 50 1 50 100)) _)
      (average _) 
      (float _))

(pprint (macroexpand '(_> (range 1000)
       (map (fn [x] (if-percent 50 1 50 100)) _)
       (average _) 
       (float _))))


;; Transliteration of PG's aif
;; Anaphoric macro that binds to "it" the test result
(defmacro aif [test then else]
  `(let [~'it ~test]
     (if ~'it ~then ~else)))

#_(aif (+ 1 1) 
        it
       :some-return)
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




