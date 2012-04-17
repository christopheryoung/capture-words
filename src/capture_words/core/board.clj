
(ns capture_words.core.board
  (:use [clojure.math.numeric-tower :only (abs)]))

(defn make-board [board-length]
  (vec (for [x (range board-length)]
         (vec (take board-length (repeat "_"))))))

(defn- first-elem-same-second-one-off [[[e1-1 e1-2] [e2-1 e2-2]]]
  (and (= e1-1 e2-1) (= 1 (abs (- e1-2 e2-2)))) )

(defn contiguous? [[elem1 elem2]]
  "Tiles are contiguous if a) they share an index; and b) they are one
step apart in their other index. elem1 and elem2 are each coordinate
pairs on a board."  
  (cond
     (first-elem-same-second-one-off [elem1 elem2]) true
     (first-elem-same-second-one-off [elem2 elem1]) true
     :else true))

(defn init-board [ & {:keys [board-length fills]
                      :or {board-length 15
                           fills 4}}]  (let [board (make-board board-length)
        pre-filled-tiles (take fills (distinct (partition 2 (repeatedly #(rand-int board-length)))))]     
    ))

(init-board)
              



