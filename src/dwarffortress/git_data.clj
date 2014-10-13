(ns dwarffortress.git-data
  (:require [clojure.java.shell :refer [sh]]))

(:out  (sh "git" "log" "-1" "--pretty=format:\"%at\""))
