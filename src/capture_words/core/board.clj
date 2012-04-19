
(ns capture_words.core.board
  (:use [clojure.math.numeric-tower :only (abs)])
  (:use [clojure.math.combinatorics :only (combinations)]))

(defn make-board [board-length board-width]
  (vec (for [x (range board-length)]
         (vec (take board-width (repeat "_"))))))

(defn neighbors? [[x y]]
  (let [up [x (+ y 1)]
        right [(+ x 1) y]
        down [x (- y 1)]
        left [(- x 1) y]]
  [up right down left]))

(defn in-coll? [elem coll]
  (boolean (some #{elem} coll)))

(defn adjacent? [[pair1 pair2]]
  (in-coll? pair1 (neighbors? pair2)))

(defn none-adjacent? [coll]
  (not-any? adjacent? (combinations coll 2)))

(defn random-letter []
  (rand-nth (map char (range (int \A) (int \Z)))))

(defn random-tile-coordinates [board-length board-width]
  "Lazy seq of random tile coordinates"
  (repeatedly #(vector (rand-int board-length) (rand-int board-width))))

(defn init-board [ & {:keys [board-length board-width pre-fills]
                      :or {board-length 15
                           board-width 15
                           pre-fills 4}}]
  (let [board (make-board board-length board-width)
        random-tile-groups (partition pre-fills (distinct (random-tile-coordinates board-length board-width)))
        non-adjacent-random-tiles (first (filter none-adjacent? random-tile-groups))
        ]
    ))

(init-board)
              



