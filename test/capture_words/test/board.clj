
(ns capture_words.test.board
  (:use [capture_words.board])
  (:use [clojure.test])
  (:use [midje.sweet]))

;; Test set up and fixtures

(defn test-board-init [board]
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
                             ]))

(def aboard (make-board :init-func test-board-init))

(fact "We have sanity checked that our fixture is set up properly"
  (at aboard [3 4] :letter) => "C")

;; Tests

(facts "Coordinates returned by coordinates-of-neighboring are
  correct and possible for the provided board"
  (coordinates-of-neighboring aboard [0 0]) =>
  (just [[0 1] [1 0]] :in-any-order)
  (coordinates-of-neighboring aboard [4 4]) =>
  (just [[4 5] [5 4] [4 3] [3 4]] :in-any-order))

(facts "neighbors? works properly"
  (neighbors? aboard [0 0] [0 1]) => true
  (neighbors? aboard [0 0] [5 5]) => false)

(facts "coordinates-all-in-a-row? returns true or false correctly"
  (coordinates-all-in-a-row? [[1 1] [1 2] [1 3] [1 4]]) => true
  ;; (coordinates-all-in-a-row? [[1 1] [1 2] [1 3] [1 5]]) => false
  ;; (coordinates-all-in-a-row? [[1 1] [2 1] [3 1] [4 1]]) => true
  ;; (coordinates-all-in-a-row? [[1 1] [2 2] [3 3] [4 4]]) => false
  )

(fact "We can 'change' the value of a tile, i.e., get a board back
with the new value in the correct place"
  (let [changed-board (change-tile-value aboard [[1 1] {:player :test-passer}])]
    (at changed-board [1 1] :player) => :test-passer))

(facts "We can 'change' multiple tile values at once"
(let [changes [[[1 1] {:player :other-test-passer}]
               [[3 4] {:letter "Z"}]]
      changed-board (change-tile-values aboard changes)]
  (at changed-board [1 1] :player) => :other-test-passer
  (at changed-board [3 4] :letter) => "Z"))

(fact "We can get a coll of all the coordinates on a board in order"
  (count (all-tile-coordinates aboard)) => 225)

(facts "We can use the commands below to look up in the board
  either with coordinates alone or with coordinates plus other
  attributes supplied."
  ((above aboard [4 4]) :letter) => "C"
  (above aboard [4 4] :letter) => "C"
  (below aboard [4 4] :letter) => "B"
  (left-of aboard [3 5] :letter) => "C"
  (right-of aboard [3 4] :letter) => "A"
  )

(facts "We get a collection of word/coordinate pairs for a particular
  coordinate on the board. Note that [2 8] has CAT running both across and
  down."
  (let [word-candidates (letter-runs-from-coordinates aboard [3 4])
        same-word-across-and-down (letter-runs-from-coordinates aboard [2 8])]
    (in-coll? word-candidates ["CAT" [[3 4] [3 5] [3 6]]]) => true
    (in-coll?  word-candidates ["CABS" [[3 4] [4 4] [5 4] [6 4]]]) => true
    (count same-word-across-and-down) => 2
    (first (first same-word-across-and-down)) => "CAT"
    (first (second same-word-across-and-down)) => "CAT"))

(fact "We can use letter-runs-from-coordinates to traverse the
  entire board, collecting all potential words"
  (let [possible-words (all-letter-runs-on-board theboard)]
    (count possible-words) => 8))

