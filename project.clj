(defproject capture_words/capture_words "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [org.clojure/math.combinatorics "0.0.2"]]
  :profiles {:dev {:dependencies [[midje "1.3.2-SNAPSHOT"]]}}
  :min-lein-version "2.0.0"
  :description "A simple word game.")