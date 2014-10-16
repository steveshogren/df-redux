(ns dwarffortress.git-data
  (:import [java.util Date]
           [java.util Calendar])
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.set :as set]
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

(defn get-day-map-from-date 
  ([d] (get-day-map-from-date d 0))
  ([d days-back]
     (let [cal (Calendar/getInstance)]
       (.setTime cal d)
       (.add cal Calendar/DATE days-back)
       {:day (.get cal Calendar/DAY_OF_YEAR)
        :date (.getTime cal)})))

(defn get-date-add-days [days]
  (get-day-map-from-date (Date.) days))

(defn dates-by-day [date-lists]
  (reduce (fn [ac ne] (assoc ac (:day ne) (:date ne)))
          {}
          date-lists))

(defn get-expected-date-map [in-past]
  (map #(get-date-add-days (- %)) (range in-past)))

(defn get-dates-from-dir [dir]
  (map get-day-map-from-date
       (filter #(= current-year (get-year %))
               (map #(from-unix-time (* 1000 (parse-long %)))
                    (get-git-logs dir)))))

(defn only-this-years [dates]
  (filter (fn [[k v]] (= current-year (get-year v))) dates))

(defn get-last-x-days [day-map num]
  (reverse (take num ((comp reverse sort) (keys day-map)))))



(defn find-missing-days [days-back]
  (let [expected-days-map (dates-by-day (get-expected-date-map days-back))
        git-dates (dates-by-day (mapcat #(get-dates-from-dir %)
                                    (conj (get-git-paths)
                                          "/home/jack/.emacs.d/.git")))
        expected-days (get-last-x-days (only-this-years expected-days-map) days-back) 
        act-days (get-last-x-days git-dates days-back)
        missing-days (set/difference (set expected-days) (set act-days))]
    (if (seq? missing-days)
      (let [earliest-missing (apply min missing-days)]
        (get expected-days-map earliest-missing)))))

(find-missing-days 20)
