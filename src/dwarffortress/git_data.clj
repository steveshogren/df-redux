(ns dwarffortress.git-data
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.tools.trace :refer [trace]]
            [dwarffortress.trace :refer [tracelet]]
            [dwarffortress.util.git :as git]
            [dwarffortress.util.dates :as dates]
            [clj-time.core :as t]
            [clj-time.local :as l]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.java.shell :refer [sh]]))

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

(defn group-dates-by-day [date-lists]
  (println "Grouping dates...")
  (reduce (fn [ac ne] (assoc ac (:day ne) (:date ne)))
          {}
          date-lists))

(defn get-expected-date-map [in-past]
  (map #(get-date-add-days (- %)) (range in-past)))

(defn get-dates-from-dir [dir]
  (map get-day-map-from-date
       (filter #(= dates/current-year (dates/get-year %))
               (map #(dates/from-unix-time (* 1000 (Long/parseLong %)))
                    (git/get-git-logs dir)))))

(defn only-this-years [dates]
  (filter (fn [[k v]] (= dates/current-year (dates/get-year v))) dates))

(defn get-last-x-days [day-map num]
  (reverse (take num ((comp reverse sort) (keys day-map)))))

(defn find-missing-days [days-back]
  (let [expected-days-map (group-dates-by-day (get-expected-date-map days-back))
        git-dates (group-dates-by-day (mapcat #(get-dates-from-dir %)
                                              (conj (git/get-git-paths)
                                                    "/home/jack/.emacs.d/.git")))
        expected-days (get-last-x-days (only-this-years expected-days-map) days-back) 
        actual-days (get-last-x-days git-dates days-back)
        missing-days (set/difference (set expected-days) (set actual-days))]
    (when (not-empty missing-days)
      (let [earliest-missing (apply min missing-days)]
        (get expected-days-map earliest-missing)))))

(defn main [& args]
  (println "Checking history...")
  (if-let [days (find-missing-days 20)]
    (println "First missing day:" days)
    (println "No gaps found"))
  (System/exit 0))
