(defproject dwarffortress "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [deft "0.1.1"]
                 [seesaw "1.4.3"]
                 [clj-time "0.8.0"]
                 [org.clojure/tools.trace "0.7.8"]
                 [midje "1.6.3"]]
  :plugins [[lein-midje "3.1.1"]
            [cider/cider-nrepl "0.7.0"]]
  :aliases {"gitcheck" ["run" "-m" "dwarffortress.git-data/main"]}
  :main dwarffortress.core)
