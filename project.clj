(defproject capture_words/capture_words "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [org.clojure/math.combinatorics "0.0.2"]
                 [lein-ring "0.8.5"]
                 [ring "1.1.8"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}}
  :plugins [[lein-midje "3.0.0"]]
  :min-lein-version "2.0.0"
  :description "A simple word game.")
