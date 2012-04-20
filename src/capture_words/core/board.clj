
(ns capture_words.core.board
  (:use [clojure.math.numeric-tower :only (abs)])
  (:use [clojure.math.combinatorics :only (combinations)]))

(defn tile []
  {:player nil
   :letter nil})

(defn make-board [board-length board-width]
  (atom (vec (for [x (range board-length)]
         (vec (take board-width (repeatedly tile)))))))

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

(defn get-tile [board coordinates]
  (get-in @board coordinates))

(defn change-tile-value [board coordinates updates]
  (let [old-tile (get-tile board coordinates)
        new-tile (merge old-tile updates)]
    (swap! board assoc-in coordinates new-tile)))

(defn init-board [ & {:keys [board-length board-width pre-fills]
                      :or {board-length 15
                           board-width 15
                           pre-fills 4}}]
  (let [board (make-board board-length board-width)
        random-tile-groups (partition pre-fills (distinct (random-tile-coordinates board-length board-width)))
        non-adjacent-random-tiles (first (filter none-adjacent? random-tile-groups))
        ]
    board))

(def board (init-board))