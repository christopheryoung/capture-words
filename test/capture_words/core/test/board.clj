
(ns capture_words.core.test.board
  (:use [capture_words.core.board])
  (:use [clojure.test]))

(deftest test-adjacent?
  (is (adjacent? [[1 1] [1 2]]))
  (is (not (adjacent? [[1 1] [1 3]])))
  (is (adjacent? [[1 1] [2 1]]))
  (is (not (adjacent? [[1 1] [3 1]]))))

(deftest test-change-tile-value?
  (let [board (init-board)]
    (change-tile-value board [1 1] {:player :blue})
    (is (get-tile board [1 1]) {:player :blue :letter nil})))

(run-all-tests)


