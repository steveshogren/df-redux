(ns dwarffortress.git-data
  (:require 
            [clojure.java.shell :refer [sh]]))

(defn from-unix-time [t]
  (java.util.Date. t))


(.parse (java.text.DateFormat/getDateInstance)
        (clojure.string/replace (:out (sh "git" "log" "-1" "--pretty=format:\"%ai\"")) "\"" "")
        


        )
