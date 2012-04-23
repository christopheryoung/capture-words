
(ns capture_words.core.test.board
  (:use [capture_words.core.board])
  (:use [clojure.test]))

(deftest test-neighbors-for-coordinates?
  (let [board (make-board)]
    (is (= (neighbors-for-coordinates board [0 0])
           [[0 1] [1 0]]))
    (is (= (neighbors-for-coordinates board [4 4])
           [[4 5] [5 4] [4 3] [3 4]]))))

(deftest test-neighbors?
  (let [board (make-board)]
    (is (= (neighbors? board [0 0] [0 1]) true))
    (is (= (neighbors? board [0 0] [5 5]) false))))

(deftest test-change-tile-value
  (let [board (make-board)
        changed-board (change-tile-value board [1 1] {:player :test-passer})]
    (is (= ((get-tile changed-board [1 1]) :player) :test-passer))))


(run-all-tests)


