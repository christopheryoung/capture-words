
(ns capture_words.utils)

;; logical relations

(defn exclusive-or [x y]
  (boolean (and (not (and x y))
                (or x y))))

(defn strictly-true-or-false? [exp]
  (or (true? exp) (false? exp)))

;; operating on collections

(defn in-coll?
  "Takes a collection and an element and returns true if the element
appears at least once in the collection; false otherwise"
  [coll elem]
  (boolean (some #{elem} coll)))

(defn all-same?
  "Takes a collection and returns true if all the items in the collection are
the same; false otherwise"
  [coll]
  (= (count (set coll)) 1))

;; Stolen straight from core.incubator
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result will
  not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn coll-of-successive-integers?
  "Takes a non-empty collection of integers and returns true if the collection
is a series of successive integers; false otherwise"
  [coll]
  (let [start (first coll)
        end (first (reverse coll))
        length (count coll)
        target-coll (range start (+ length start))]
    (= coll target-coll)))


