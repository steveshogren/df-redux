(ns dwarffortress.util.git
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

(defn get-git-paths []
  (map #(.getPath %)
       (filter (fn [x] (and (.isDirectory x)
                            (.endsWith (.getName x) ".git")))
               (file-seq (io/file "/home/jack/programming")))))

(defn get-git-logs [place]
  (-> (sh/sh "git"
          (str "--git-dir=" place)
          "log" ;; "-7"
          "--pretty=format:\"%at\"")
      :out 
      (s/replace "\"" "")
      (s/split #"\n")))
