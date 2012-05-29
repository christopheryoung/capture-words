
(ns capture_words.word_utils
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn letter-probability-map
  "Returns probabilities of letters based on frequencies in the language"
  []
  {"E" 12.02
   "T" 9.10
   "A" 8.12
   "O" 7.68
   "I" 7.31
   "N" 6.95
   "S" 6.28
   "R" 6.02
   "H" 5.92
   "D" 4.32
   "L" 3.98
   "U" 2.88
   "C" 2.71
   "M" 2.61
   "F" 2.30
   "Y" 2.11
   "W" 2.09
   "G" 2.03
   "P" 1.82
   "B" 1.49
   "V" 1.11
   "K" 0.69
   "X" 0.17
   "Q" 0.11
   "J" 0.10
   "Z" 0.07})

(defn- upper-case-and-conj [coll x]
  (conj coll (string/upper-case x)))

(defn get-all-words [file-name]
  (with-open [rdr (clojure.java.io/reader file-name)]
    (reduce upper-case-and-conj [] (line-seq rdr))))

(def all-words (future (get-all-words "src/capture_words/dicts/english.txt")))

(defn- select-random-letter
  "For now, just selects a random letter. Later, should provide
letters in frequences observed in the language."
  []
  (str (char (rand-nth (range (int \A) (int \Z))))))

(defn letters
  "Lazy sequence of letters with individual frequencies in
  proproportion to their frequences in the word-list"
  []
  (seq (repeatedly select-random-letter)))

(defn word?
  "Inefficient stub for a func that takes a word and returns true or
  false depending on whether it is a word"
  [word]
  (let [upper-word (string/upper-case word)]
    (boolean (some #{upper-word} @all-words))))