(defproject capture_words/capture_words "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [org.clojure/math.combinatorics "0.0.2"]
                 [lein-ring "0.7.0"]
                 [ring "1.1.0"]
                 [sircyb-utils "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[midje "1.4.0-RC1"]]}}
  :min-lein-version "2.0.0"
  :repositories {"local" ~(str (.toURI (java.io.File. "maven_repo")))}
  :description "A simple word game.")