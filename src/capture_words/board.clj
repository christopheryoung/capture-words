
(ns capture_words.board
  (:use [clojure.math.numeric-tower :only (abs)])
  (:use [clojure.math.combinatorics :only (combinations)])
  (:use [clojure.set :only (intersection union difference)])
  (:use [capture_words.word_utils :only (word?)])
  (:use [sircyb-utils.core :only (exclusive-or
                                  strictly-true-or-false?
                                  in-coll?
                                  all-same?
                                  coll-of-successive-integers?)]))

;; General utilities

(defn throw-if-false [status excp]
  (if-not (strictly-true-or-false? status)
    (throw (Exception.
            (format "A status must evaluate to either strictly true or false,
            but status provided was: %s" status))))
  (if-not status
    (throw excp)))

(defn summarize-check-results
  "Takes alternating checks and exceptions, where the check is the condition to
be tested which evaluates to either true or false and the exception is what is
returned in the case that the check returns false. Only throws the first
exception encountered. Throws exception if any of the checks evaluate to
neither strictly true or false."
  [ & check-and-exception-pairs]
  (if (seq check-and-exception-pairs)
    (let [status (first check-and-exception-pairs)
          excp (second check-and-exception-pairs)]
      (throw-if-false status excp)
      (recur (nnext check-and-exception-pairs)))))

;; The board and its pieces . . .

(defn tile []
  {:player nil
   :letter nil})

(defn make-board
  "Returns a board using provided funcs"
  [ & {:keys [board-length board-width init-func tile-func]
       :or {board-length 15
            board-width 15
            init-func identity
            tile-func tile}}]
  (let [board (vec (for [x (range board-length)]
                     (vec (take board-width (repeatedly tile-func)))))]
    (init-func board)))

(defn initial-board []
  (make-board))

;; Iterating through the board's coordinates

(defn all-tile-coordinates
  "A flattened list of tile coordinates"
  [board]
  (for [length-wise (range (count board))
        width-wise (range (count (first board)))]
    (vector length-wise width-wise)))

;; Locating on the board . . .

(defn coord-above [[x y]] [(dec x) y])

(defn coord-right-of [[x y]] [x (inc y)])

(defn coord-below [[x y]] [(inc x ) y])

(defn coord-left-of [[x y]] [x (dec y)])

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
  (for [x (range x (inc (count board)))] [x y]))

(defn legal-coordinate [board coordinates]
  (or (get-in board coordinates) false))

(defn coordinates-of-neighboring
  "Takes a board and a set of coordinates and returns all legal
neighbors for those coordinates on the given board."
  [board coordinates]
  (filter #(legal-coordinate board %) [(coord-above coordinates)
                                       (coord-right-of coordinates)
                                       (coord-below coordinates)
                                       (coord-left-of coordinates)]))

(defn coordinates-of-neighborings
  "Takes a vector of coordinates and gives all the legal neighbors, including
the original coordinates"
  [board coordinates-vec]
  (let [neighbors-for-each (map (partial coordinates-of-neighboring board) coordinates-vec)]
    (reduce concat [] neighbors-for-each)))

(defn neighbors?
  "Takes a board and two coordinate pairs and returns true or false
depending on whether the coordinate pairs are neighbors on the board."
  [board pair1 pair2]
  (in-coll? (coordinates-of-neighboring board pair2) pair1))

(defn coordinates-all-in-a-row? [coordinates-vec]
  (let [xs (sort (map first coordinates-vec))
        ys (sort (map second coordinates-vec))]
    (or (and (coll-of-successive-integers? xs) (all-same? ys))
        (and (coll-of-successive-integers? ys) (all-same? xs)))))

;; "Changing" tile values

(defn change-tile-value
  "Returns a new board featuring the updates to the tile at the given
coordinates."
  [board [coordinates updates]]
  (let [old-tile (at board coordinates)
        new-tile (merge old-tile updates)]
    (assoc-in board coordinates new-tile)))

(defn change-tile-values [board changes]
  (reduce change-tile-value board changes))

(defn- changes-for-coordinates
  "change-tile-value expects a sequence of coordinates and updates. This helper
  method takes a set of such coordinates and a single change and returns the
  changes in the expected format."
  [coordinate-sets change]
  (partition 2 (interleave coordinate-sets (repeat change))))

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

(defn letter-runs-from-coordinates
  "Returns a vector of letter-runs (possible words) coordinates pairs
  for those letter-runs. To avoid double counting, when we find a
  letter, we check if it has a letter to its left. If it doesn't, we
  add the row of letters to its right, if any. If it does, it has
  already been counted horizontally. We also check if it has a letter
  above it. If it does, it has already been counted vertically. If it
  doesn't, we add the row of letters below it, if any."
  [board coordinates]
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

(defn all-letter-runs-on-board
  "Returns seq of pairs of word candidates/coordinates vectors on the
  board. We're looking for all the ways we can make possibly legal
  words out of the combination of letters on the board. We do this by
  scanning the board from left to right and top to bottom and asking
  of each tile whether it has any elligible letter runs. "
  [board]
  (let [coordinates-to-search (all-tile-coordinates board)
        all-letter-runs (mapcat #(letter-runs-from-coordinates board %) coordinates-to-search)]
    (filter #(not (empty? %)) all-letter-runs)))

(defn illegal-words-on-board
  "Does the given board consist entirely of words (or are there illegal
groupings of characters?"
  [board]
  (let [candidates (map first (all-letter-runs-on-board board))]
    (filter #(not (word? %)) candidates)))

(defn- all-letters-connected-recur?
  "Recursive function used internally by all-letters-connected."
  [board collector remaining-letter-runs]
  (let [neighbor-coordinates-groups (map #(coordinates-of-neighboring board %) collector)
        neighbor-coordinates (set (apply concat neighbor-coordinates-groups))
        neighbors-in-remaining (intersection (set remaining-letter-runs) neighbor-coordinates)
        new-collector (union collector neighbors-in-remaining)
        new-remaining (difference remaining-letter-runs neighbors-in-remaining)]
    (cond
     (empty? remaining-letter-runs) true
     (empty? neighbors-in-remaining) false
     :else (all-letters-connected-recur? board new-collector new-remaining))))

(defn all-letters-connected?
  "Takes a set of coordinates and returns true if they're all connected."
  [board coordinates]
  (let [collector (set (first coordinates))
        remaining (set (apply concat (rest coordinates)))]
    (all-letters-connected-recur? board collector remaining)))

(defn possible-move?
  "Takes a board and proposed changes, which are a list of
 coordinate/attributes pairs. Returns nil on success or throws an Exception if
 the move is illegal:

Here, we are only checking:

1. Do the changes place letters in a legal location? For this check, we apply
three rules: If there are no letters on the board, the letters can be played
anywhere. Otherwise, they must be attached to letters already on the board. (We
only need to actually check the latter, of course.) b) Letters cannot be placed
on top of tiles that already have letters. c) Letters must be played in a
straight line.

2. As a result of the change, are all the letter-combinations on the
board actual words?

Note: This function does not check the validity of the move. So, we
are NOT checking here to ensure that i) it is the player's turn ii)
the player is submitting a move on her own behalf; iii) that the
changes are possible for the player, i.e., that she actually has the
letters available to her to play on the board. These checks are done
elsewhere."
  [board changes]
  (let [change-coordinates (map first changes)
        coordinates-of-letters-already-on-board (filter #(at board % :letter) (all-tile-coordinates board))
        overlapping-letters (intersection
                             (set change-coordinates)
                             (set coordinates-of-letters-already-on-board))
        board-after-move (change-tile-values board changes)
        all-letter-runs-after-move (all-letter-runs-on-board board-after-move)
        coordinates-of-all-letter-runs-after-move (map second all-letter-runs-after-move)
        illegal-words-after-move (illegal-words-on-board board-after-move)
        letters-connected (all-letters-connected?
                           board-after-move
                           coordinates-of-all-letter-runs-after-move)
        ]
    (summarize-check-results
     (empty? overlapping-letters) (Exception. "Letters overlap")
     (coordinates-all-in-a-row? change-coordinates) (Exception. "Letters are non-contiguous")
     letters-connected (Exception. "Letters are disconnected")
     (empty? illegal-words-after-move) (Exception. "Not a word"))))

(defn- update-tile-ownership
  "We transfer ownership of all the tiles played by the player to the player
  along with the immediate neighbours of all the tiles played."
  [board changes player]
  (let [board-after-move (change-tile-values board changes)
        coordinates-of-change (map first changes)
        possibly-affected-coordinates (coordinates-of-neighborings board coordinates-of-change)
        affected-neighbors (filter #(at board % :letter) possibly-affected-coordinates)
        player-changes (changes-for-coordinates (concat coordinates-of-change affected-neighbors) {:player player})]
    (change-tile-values board player-changes)))

(defn player-score [board player]
  (count (filter #(= (at board % :player) player) (all-tile-coordinates board))))

(defn score-move
  "We score a move by taking the difference between the number of tiles owned
  by the player on the board prior to the move and the number of tiles that
  would be owned after the move."
  [board changes player]
  (let [board-after-move (update-tile-ownership board changes player)]
   (- (player-score board-after-move player) (player-score board player))))

(defn make-move [board changes player]
  (possible-move? board changes) ;; throws exception if not possible
  (let [board-with-updated-letter (change-tile-values board changes)]
    (update-tile-ownership board-with-updated-letter changes player)))

(defn the-board-init [board]
  (change-tile-values board [
                             [[2 8] {:player :A :letter "C"}]
                             [[2 9] {:player :A :letter "A"}]
                             [[2 10] {:player :A :letter "T"}]
                             [[3 4] {:player :A :letter "C"}]
                             [[3 5] {:player :A :letter "A"}]
                             [[3 6] {:player :A :letter "T"}]
                             [[3 8] {:player :A :letter "A"}]
                             [[4 4] {:player :A :letter "A"}]
                             [[4 6] {:player :A :letter "O"}]
                             [[4 7] {:player :A :letter "T"}]
                             [[4 8] {:player :A :letter "T"}]
                             [[4 9] {:player :A :letter "E"}]
                             [[4 10] {:player :A :letter "R"}]
                             [[5 4] {:player :A :letter "B"}]
                             [[5 5] {:player :A :letter "O"}]
                             [[5 6] {:player :A :letter "Y"}]
                             [[6 3] {:player :A :letter "I"}]
                             [[6 4] {:player :A :letter "S"}]
                             [[7 4] {:player :A :letter "T"}]
                             [[8 4] {:player :A :letter "A"}]
                             [[9 4] {:player :A :letter "N"}]
                             [[10 4] {:player :A :letter "D"}]
                             ]))


