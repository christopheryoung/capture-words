
(ns capture_words.core.test.board
  (:use [capture_words.core.board])
  (:use [clojure.test]))

(defn board-fixture [f]
  (binding [aboard (atom (make-board))])
  (f))

(deftest test-neighbors-for-coordinates?
  (is (= (neighbors-for-coordinates aboard [0 0])
         [[0 1] [1 0]]))
  (is (= (neighbors-for-coordinates aboard [4 4])
         [[4 5] [5 4] [4 3] [3 4]])))

(deftest test-neighbors?
  (is (= (neighbors? aboard [0 0] [0 1]) true))
  (is (= (neighbors? aboard [0 0] [5 5]) false)))

(deftest test-change-tile-value
  (let [changed-board (change-tile-value aboard [1 1] {:player :test-passer})]
    (is (= ((get-tile changed-board [1 1]) :player) :test-passer))))

(run-all-tests)


