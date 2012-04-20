
(ns capture_words.core.test.board
  (:use [capture_words.core.board])
  (:use [clojure.test]))

(deftest test-adjacent?
  (is (adjacent? [[1 1] [1 2]]))
  (is (not (adjacent? [[1 1] [1 3]])))
  (is (adjacent? [[1 1] [2 1]]))
  (is (not (adjacent? [[1 1] [3 1]]))))

(deftest test-change-tile-value
  (let [test-board (init-board)]
    (is (= (get-tile test-board [1 1]) {:player nil :letter nil}))
    (change-tile-value test-board [1 1] {:player :test-passer})
    (is (= (get-tile test-board [1 1]) {:player :test-passer :letter nil}))))

(run-all-tests)


