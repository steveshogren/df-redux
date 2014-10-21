(ns dwarffortress.util.dates
  (:import [java.util Date]
           [java.util Calendar]))

(defn from-unix-time [t]
  (java.util.Date. t))

(defn get-year [d]
  (let [cal (Calendar/getInstance)]
    (.setTime cal d)
    (.get cal java.util.Calendar/YEAR)))

(def current-year (get-year (Date.)))

(defn add-days-to-date [days date]
  (let [cal (Calendar/getInstance)]
    (.setTime cal date)
    (.add cal Calendar/DATE days)
    (.setTimeZone cal (java.util.TimeZone/getDefault))
    (let [expected-day (.get cal Calendar/DATE)
          inc-time-by (rand-int 60)]
      ;; (.add cal java.util.Calendar/MINUTE inc-time-by)
      (.getTime cal))))
