
(ns capture_words.core.test.board
  (:use [capture_words.core.board])
  (:use [clojure.test]))

;; Test set up and fixtures

(defn test-board-init [board]
  (change-tile-values board [[[3 4] {:player :A :letter "C"}]
                             [[3 5] {:player :A :letter "A"}]
                             [[3 6] {:player :A :letter "T"}]
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
  (is (= (get-in aboard [3 4 :letter]) "C")))

(deftest test-neighbors-for-coordinates?
  (is (= (neighbors-for-coordinates aboard [0 0])
         [[0 1] [1 0]]))
  (is (= (neighbors-for-coordinates aboard [4 4])
         [[4 5] [5 4] [4 3] [3 4]])))

(deftest test-neighbors?
  (is (= (neighbors? aboard [0 0] [0 1]) true))
  (is (= (neighbors? aboard [0 0] [5 5]) false)))

(deftest test-change-tile-value
  (let [changed-board (change-tile-value aboard [[1 1] {:player :test-passer}])]
    (is (= ((get-tile changed-board [1 1]) :player) :test-passer))))

(deftest test-change-tile-values
  (let [changes [[[1 1] {:player :other-test-passer}]
                 [[1 2] {:player :test-passer}]]
        changed-board (change-tile-values aboard changes)]
    (is (= ((get-tile changed-board [1 1]) :player) :other-test-passer))
    (is (= ((get-tile changed-board [1 2]) :player) :test-passer))))

(run-all-tests)


