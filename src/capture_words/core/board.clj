
(ns capture_words.core.board
  (:use [clojure.math.numeric-tower :only (abs)]))

(defn make-board [board-length]
  (vec (for [x (range board-length)]
         (vec (take board-length (repeat "_"))))))

(defn neighbors? [[x y]]
  (let [up [x (+ y 1)]
        right [(+ x 1) y]
        down [x (- y 1)]
        left [(- x 1) y]]
  [up right down left]))

(defn in-coll? [elem coll]
  (or
   (and (some #{elem} coll) true)
   false))

(defn adjacent? [[pair1 pair2]]
  (in-coll? pair1 (neighbors? pair2)))

(defn init-board [ & {:keys [board-length fills]
                      :or {board-length 15
                           fills 4}}]
  (let [board (make-board board-length)
        pre-filled-tiles (take fills (distinct (partition 2 (repeatedly #(rand-int board-length)))))
        ]
    ))

(init-board)
              



