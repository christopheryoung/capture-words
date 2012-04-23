
(ns capture_words.word_utils.core
  (:import (java.io BufferedReader FileReader)))

(defn get-dict [file-name]
  )

(defn- build-words [words word]
  )

(defn process-file [file-name line-func line-acc]
  (let [word {}
        ]
    (with-open [rdr (BufferedReader. (FileReader. file-name))]
      (reduce  (line-seq rdr))))
  )

(def dict (get-dict "src/capture_words/word_utils/dicts/english.txt"))

dict

