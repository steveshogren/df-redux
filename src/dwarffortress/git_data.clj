(ns dwarffortress.git-data
  (:import [java.util Date]
           [java.util Calendar])
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

(defn from-unix-time [t]
  (java.util.Date. t))
(defn parse-long [s]
  (Long/parseLong s))

(defn get-git-paths []
  (map #(.getPath %)
       (filter (fn [x] (and (.isDirectory x)
                            (.endsWith (.getName x) ".git")))
               (file-seq (io/file "/home/jack/programming")))))

(defn get-git-logs [place]
  (-> (sh "git"
          (str "--git-dir=" place)
          "log" ;; "-7"
          "--pretty=format:\"%at\"")
      :out 
      (s/replace "\"" "")
      (s/split #"\n")))

(defn get-year [d]
  (let [cal (Calendar/getInstance)]
    (.setTime cal d)
    (.get cal java.util.Calendar/YEAR)))

(def current-year (get-year (Date.)))

(defn get-day-from-date
  ([d] (get-day-from-date d 0))
  ([d days-back]
     (let [cal (Calendar/getInstance)]
       (.setTime cal d)
       (.add cal Calendar/DATE days-back)
       {:day (.get cal Calendar/DAY_OF_YEAR)
        :date (.getTime cal)})))

(defn get-date-add-days [days]
  (get-day-from-date (Date.) days))

(defn dates-by-day [date-lists]
  (reduce (fn [ac ne] (assoc ac (:day ne) (:date ne)))
          {}
          date-lists))

(defn get-expected-date-map [in-past]
  (map #(get-date-add-days (- %)) (range in-past)))

(defn get-dates-from-dir [dir]
  (map get-day-from-date
       (filter #(= current-year (get-year %))
               (map #(from-unix-time (* 1000 (parse-long %)))
                    (get-git-logs dir)))))

(def git-d (dates-by-day (mapcat #(get-dates-from-dir %)
                                 (conj (get-git-paths)
                                       "/home/jack/.emacs.d/.git")))) 
(def expect-d (dates-by-day (get-expected-date-map 15))) 

(defn only-this-years [dates]
  (filter (fn [[k v]] (= current-year (get-year v))) dates))

(defn get-last-x-days [day-map num]
  (reverse (take num ((comp reverse sort) (keys day-map)))))

(let [expected-days (get-last-x-days (only-this-years expect-d) 15) 
      act-days (get-last-x-days git-d 15)]
  [expected-days act-days]
  )
(keys (filter (fn [[day dates]]
                (= 1 (count dates))) ))
(group-by (fn [x] x) 
          (concat (range 5 10)
                  [1 3 5 7 10]))
(into (set (range 5 10)) [1 3 5 7 10])
