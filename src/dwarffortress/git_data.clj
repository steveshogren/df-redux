(ns dwarffortress.git-data
  (:import [java.util Date]
           [java.util Calendar])
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

(defn from-unix-time [t]
  (java.util.Date. t))
(defn parseLong [s]
  (Long/parseLong s))

(defn get-git-paths []
  (map #(.getPath %)
       (filter (fn [x] (and (.isDirectory x)
                            (.endsWith (.getName x) ".git")))
               (file-seq (io/file "/home/jack/programming")))))

(defn getGitLogs [place]
  (s/split (s/replace (:out (sh "git"
                                (str "--git-dir=" place)
                                "log" "-7"
                                "--pretty=format:\"%at\""))
                      "\"" "")
           #"\n"))

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
  (reduce (fn [ac ne] 
            (assoc ac (:day ne) (:date ne)))
          {}
          date-lists))

(defn get-expected-date-map [in-past]
  (map #(get-date-add-days (- %)) (range in-past)))

(def git-d (map get-day-from-date (map #(from-unix-time (* 1000 (parseLong %)))
                                       (getGitLogs "/home/jack/programming/dwarffortress/.git/")))) 

(def expect-d (dates-by-day (get-expected-date-map 10))) 

(dates-by-day git-d)

