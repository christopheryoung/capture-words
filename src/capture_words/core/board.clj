
(ns capture_words.core.board
  (:use [clojure.math.numeric-tower :only (abs)])
  (:use [clojure.math.combinatorics :only (combinations)]))

(defn tile []
  {:player nil
   :letter nil})

(defn legal-coordinates-for-board? [board]
  "Returns a function that takes a set of coordinates and returns true
  if they are legal on the board and false if they are not."
  (fn [[x y]]
    (try
      ((board x) y)
      true
      (catch IndexOutOfBoundsException e false))))

(defn neighbors-for-coordinates [board [x y]]
  "Takes a board and a set of coordinates and returns all legal
neighbors for those coordinates on the given board."
  (let [up [x (+ y 1)]
        right [(+ x 1) y]
        down [x (- y 1)]
        left [(- x 1) y]
        legal? (legal-coordinates-for-board? board)]
    (filter legal? [up right down left])))

(defn in-coll? [elem coll]
  (boolean (some #{elem} coll)))

(defn neighbors? [board pair1 pair2]
  "Takes a board and two coordinate pairs and returns true or false
depending on whether the coordinate pairs are neighbors on the board."
  (in-coll? pair1 (neighbors-for-coordinates board pair2)))

(defn get-tile [board coordinates]
  "Returns the tile at the given coordinates"
  (get-in board coordinates))

(defn change-tile-value [board coordinates updates]
  "Returns a new board featuring the updates to the tile at the given
coordinates."
  (let [old-tile (get-tile board coordinates)
        new-tile (merge old-tile updates)]
    (assoc-in board coordinates new-tile)))

(defn make-board [ & {:keys [board-length board-width init-func tile-func]
                      :or {board-length 15
                           board-width 15
                           init-func identity
                           tile-func tile}}]
  "Returns a board using provided funcs"
  (let [board (vec (for [x (range board-length)]
                     (vec (take board-width (repeatedly tile-func)))))]
    (init-func board)))
