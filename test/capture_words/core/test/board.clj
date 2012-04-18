
(ns capture_words.core.test.board
  (:use [capture_words.core.board :only (adjacent?)])
  (:use [clojure.test]))

(deftest test-adjacent?
  (is (adjacent? [[1 1] [1 2]]))
  (is (not (adjacent? [[1 1] [1 3]])))
  (is (adjacent? [[1 1] [2 1]]))
  (is (not (adjacent? [[1 1] [3 1]]))))

(run-all-tests)


