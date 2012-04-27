
(ns capture_words.board
  (:use [clojure.math.numeric-tower :only (abs)])
  (:use [clojure.math.combinatorics :only (combinations)])
  (:use [capture_words.word_utils :only (word?)]))

;; General utility (to be moved into separate library)

(defn in-coll? [coll elem]
  "Takes a collection and an element and returns true if the element
appears at least once in the collection"
  (boolean (some #{elem} coll)))

(defn exclusive-or [x y]
  (boolean (and (not (and x y))
                (or x y))))

(defn all-same? [coll]
  "Takes a collection and returns true if all the items in the
collection are the same; false otherwise"
  (= (count (set coll)) 1))

(defn coll-of-successive-integers? [coll]
  "Takes a collection of integers and returns true if the collection
is a non-empty series of successive integers"
  (let [start (first coll)
        end (first (reverse coll))
        length (count coll)]
    (and (not (empty? coll))
         (= coll (range start (+ length 1))))))

;; The board and its pieces . . .

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

;; Iterating through the board's coordinates

(defn all-tile-coordinates [board]
  "A flattened list of tile coordinates"
  (for [length-wise (range (count board))
        width-wise (range (count (first board)))]
    (vector length-wise width-wise)))

;; Locating on the board . . .

(defn coord-above [[x y]] [(- x 1) y])

(defn coord-right-of [[x y]] [x (+ y 1)])

(defn coord-below [[x y]] [(+ x 1) y])

(defn coord-left-of [[x y]] [x (- y 1)])

(defn loc [rule]
  (fn [board coordinates & attrs]
    (let [return-coordinates (rule coordinates)
          return-coordinates-and-attrs (if attrs (concat return-coordinates attrs) return-coordinates)]
      (get-in board return-coordinates-and-attrs))))

(def at (loc identity))

(def above (loc coord-above))

(def right-of (loc coord-right-of))

(def below (loc coord-below))

(def left-of (loc coord-left-of))

(defn to-the-right-from [board [x y]]
  (for [y (range y (count (first board)))] [x y]))

(defn to-the-bottom-from [board [x y]]
  (for [x (range x (+ (count board) 1))] [x y]))

(defn legal-coordinate [board coordinates]
  (or (get-in board coordinates) false))

(defn coordinates-of-neighboring [board coordinates]
  "Takes a board and a set of coordinates and returns all legal
neighbors for those coordinates on the given board."
  (filter #(legal-coordinate board %) [(coord-above coordinates)
                                       (coord-right-of coordinates)
                                       (coord-below coordinates)
                                       (coord-left-of coordinates)]))

(defn neighbors? [board pair1 pair2]
  "Takes a board and two coordinate pairs and returns true or false
depending on whether the coordinate pairs are neighbors on the board."
  (in-coll? (coordinates-of-neighboring board pair2) pair1))

(defn coordinates-all-in-a-row? [coordinates-vec]
  (let [xs (sort (map first coordinates-vec))
        ys (sort (map second coordinates-vec))]
    (sequential? xs)
    ;;(and (sequential? xs) (all-same? ys))
    ;; (or (and (sequential? xs) (all-same? ys))
    ;;     (and (sequential? ys (all-same? xs))))
    ))

(coordinates-all-in-a-row? [[1 1] [1 2] [1 3]])

;; "Changing" tile values

(defn change-tile-value [board [coordinates updates]]
  "Returns a new board featuring the updates to the tile at the given
coordinates."
  (let [old-tile (at board coordinates)
        new-tile (merge old-tile updates)]
    (assoc-in board coordinates new-tile)))

(defn change-tile-values [board changes]
  (reduce change-tile-value board changes))

;; Working with letters on tiles

(defn word-starting [direction]
  (fn [board coordinates]
    (let [coordinates-in-direction (direction board coordinates)
          letter-values (map #(at board % :letter) coordinates-in-direction)
          letters (take-while #(not (nil? %)) letter-values)
          possible-word (clojure.string/join "" letters)
          word-coordinates (subvec (vec coordinates-in-direction) 0 (count letters))]
      {possible-word word-coordinates})))

;; usage: (horizontal-word-starting board coordinates)
(def horizontal-word-starting (word-starting to-the-right-from))

;; usage: (vertical-word-starting board coordinates)
(def vertical-word-starting (word-starting to-the-bottom-from))

(defn letter-runs-from-coordinates [board coordinates]
  "Returns a vector of letter-runs (possible words) coordinates pairs
  for those letter-runs. To avoid double counting, when we find a
  letter, we check if it has a letter to its left. If it doesn't, we
  add the row of letters to its right, if any. If it does, it has
  already been counted horizontally. We also check if it has a letter
  above it. If it does, it has already been counted vertically. If it
  doesn't, we add the row of letters below it, if any."
  (concat []
         (if (and
              (at board coordinates :letter)
              (not (left-of board coordinates :letter))
              (right-of board coordinates :letter))
           (horizontal-word-starting board coordinates)
           [])

         (if (and
              (at board coordinates :letter)
              (not (above board coordinates :letter))
              (below board coordinates :letter))
           (vertical-word-starting board coordinates)
           [])))

(defn all-letter-runs-on-board [board]
  "Returns seq of pairs of word candidates/coordinates vectors on the
  board. We're looking for all the ways we can make possibly legal
  words out of the combination of letters on the board. We do this by
  scanning the board from left to right and top to bottom and asking
  of each tile whether it has any elligible letter runs. "
  (let [coordinates-to-search (all-tile-coordinates board)
        all-letter-runs (mapcat #(letter-runs-from-coordinates board %) coordinates-to-search)]
    (filter #(not (empty? %)) all-letter-runs)))

(defn board-all-words? [board]
  "Does the given board consist entirely of words (or are there illegal groupings of characters?"
  (let [candidates (map first (all-letter-runs-on-board board))]
    candidates))

(defn has-neighbor-with-letter? [board coordinates]
  (let [letters-in-neighboring (filter #(not (nil? %))
                                       (map #(at board coordinates :letter)
                                            (coordinates-of-neighboring board coordinates)))]
    ((not (empty? letters-in-neighboring))))
  )

(defn possible-move? [board changes]
  "Takes a board and proposed changes, which are a list of
 coordinate/attributes pairs. Returns true or false.

Here, we are only checking:

1. Do the changes place letters in a legal location? For this check,
we apply three rules: a) If there are no letters on the board, the
letters can be played anywhere. Otherwise, they must be attached to
letters already on the board. b) Letters cannot be placed on top of
tiles that already have letters. c) Letters must be played in a
straight line.

2. As a result of the change, are all the letter-combinations on the
board actual words?

Note: This function does not check the validity of the move. So, we
are NOT checking here to ensure that i) it is the player's turn ii)
the player is submitting a move on her own behalf; iii) that the
changes are possible for the player, i.e., that she actually has the
letters available to her to play on the board. These checks are done
elsewhere."
  ;; (let [has-letter? (fn [coordinates] (if (at board coordinates :letter) coordinates))
  ;;       coordinates-of-letters-already-on-board (filter has-letter? (all-tile-coordinates board))
  ;;       board-after-move (change-tile-values board changes)
        
  ;;       coordinates-of-letters-after-move (filter has-letter? (all-tile-coordinates board-after-move))
  ;;       isolated-letters-after-move (filter has-neighbor-with-letter? (coordinates-of-letters-after-move))
  ;;       word-candidates-after-move (map )
  ;;       ]
  ;;   (and
  ;;    (or (empty? coordinates-of-letters-already-on-board) (empty? isolated-letters-after-move))
  ;;    ())
  ;;   )
  )

(defn score-move [board changes])
