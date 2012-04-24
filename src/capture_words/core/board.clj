
(ns capture_words.core.board
  (:use [clojure.math.numeric-tower :only (abs)])
  (:use [clojure.math.combinatorics :only (combinations)]))

(defn tile []
  {:player nil
   :letter nil})

(defn make-board [ & {:keys [board-length board-width init-func tile-func]
                      :or {board-length 15
                           board-width 15
                           init-func identity
                           tile-func tile}}]
  "Returns a board using provided funcs"
  (let [board (vec (for [x (range board-length)]
                     (vec (take board-width (repeatedly tile-func)))))]
    (init-func board)))

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

(defn change-tile-value [board [coordinates updates]]
  "Returns a new board featuring the updates to the tile at the given
coordinates."
  (let [old-tile (get-tile board coordinates)
        new-tile (merge old-tile updates)]
    (assoc-in board coordinates new-tile)))

(defn change-tile-values [board changes]
  (reduce change-tile-value board changes))

(defn tile-to-top [board [x y]]
  (get-in board [(+ x 1) y]))

(defn tile-to-right [board [x y]]
  (get-in board [x (+ y 1)]))

(defn tile-to-bottom [board [x y]]
  (get-in board [x (- y 1)]))

(defn tile-to-left [board [x y]]
  (get-in board [(- x 1) y]))

(defn tile-coordinates-in-board [board]
  "A flattened list of tile coordinates"
  (for [length-wise (range (count board))
        width-wise (range (count (first board)))]
    (vector length-wise width-wise)))

(defn all-letter-runs-on-board [board]
  "We're looking for all the ways we can make possibly legal words out
  of the combination of letters on the board. We do this by scanning
  the board from left to right and top to bottom. When we find a
  letter, we check if it has a letter above it. If it doesn't, we add
  the column of letters below it, if any. If it does, it has already been
  counted vertically. We also check if it has a letter to its left. If
  it does, it has already been counted horizontally. If it doesn't, we
  add the row of letters to its right, if any."
  )

(defn board-all-words? [board]
  "Does the given board consist entirely of words (or are there illegal groupings of characters?"
  )
