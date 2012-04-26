
(ns capture_words.core.test.board
  (:use [capture_words.core.board])
  (:use [clojure.test]))

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

(defn board-fixture [f]
  (def ^:dynamic aboard (make-board :init-func test-board-init))
  (f))

(use-fixtures :each board-fixture)

;; Tests

(deftest test-fixtures
  (is (= (at aboard [3 4] :letter) "C")))

(deftest test-neighbors-for-coordinates?
  (testing "Coordinates returned by coordinates-of-neighboring are
  correct and possible for the provided board"
    (is (= (set (coordinates-of-neighboring aboard [0 0]))
           (set [[0 1] [1 0]])))
    (is (= (set (coordinates-of-neighboring aboard [4 4]))
       (set [[4 5] [5 4] [4 3] [3 4]])))))

(deftest test-neighbors?
  (is (= (neighbors? aboard [0 0] [0 1]) true))
  (is (= (neighbors? aboard [0 0] [5 5]) false)))

(deftest test-change-tile-value
  (let [changed-board (change-tile-value aboard [[1 1] {:player :test-passer}])]
    (is (= (at changed-board [1 1] :player) :test-passer))))

(deftest test-change-tile-values
  (let [changes [[[1 1] {:player :other-test-passer}]
                 [[3 4] {:letter "Z"}]]
        changed-board (change-tile-values aboard changes)]
    (is (= (at changed-board [1 1] :player) :other-test-passer))
    (is (= (at changed-board [3 4] :letter) "Z"))))

(deftest test-all-tile-coordinates
  (is (= (count (all-tile-coordinates aboard)) 225)))

(deftest test-tile-directions
  (testing "We can use the commands below to look up in the board
  either with coordinates alone or with coordinates plus other
  attributes supplied."
  (is (= ((above aboard [4 4]) :letter) "C"))
  (is (= (above aboard [4 4] :letter) "C"))
  (is (= (below aboard [4 4] :letter) "B"))
  (is (= (left-of aboard [3 5] :letter) "C"))
  (is (= (right-of aboard [3 4] :letter) "A"))))

(deftest test-letters-runs-from-coordinate
  (testing "We get a collection of word/coordinate pairs for a particular coordinate on the board"
    (let [word-candidates (letter-runs-from-coordinates aboard [3 4])
          same-word-across-and-down (letter-runs-from-coordinates aboard [2 8])]
      (is (in-coll? word-candidates ["CAT" [[3 4] [3 5] [3 6]]]))
      (is (in-coll?  word-candidates ["CABS" [[3 4] [4 4] [5 4] [6 4]]]))
      ;; [2 8] has CAT running across and down
      (is (= (count same-word-across-and-down) 2))
      (is (= (first (first same-word-across-and-down)) "CAT"))
      (is (= (first (second same-word-across-and-down)) "CAT"))
      )))

(deftest test-all-letter-runs-on-board
  (testing "We can use letter-runs-from-coordinates to traverse the
  entire board, collecting all potential words"
    (let [possible-words (all-letter-runs-on-board theboard)]
      (is (= (count possible-words) 8)))))

(run-all-tests)