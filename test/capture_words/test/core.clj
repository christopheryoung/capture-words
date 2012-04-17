
(ns capture_words.test.core
  (:use [capture_words.core.board :only (contiguous?)])
  (:use [clojure.test]))

(deftest test-contiguous?
  (is (contiguous? [[1 1] [1 2]]))
  (is (not (contiguous? [[1 1] [1 3]])))
  (is (contiguous? [[1 1] [2 1]]))
  (is (not (contiguous? [[1 1] [3 1]]))))



(run-all-tests)


