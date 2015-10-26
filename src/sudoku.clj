(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (get-in board coord))))


(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set
     (for [r board]
       (get r col)))))

(defn coord-pairs [coords]
  (for [row coords]
    (for [col coords]
      [row col])))

(coord-pairs [6 7])

(defn coords-block [row col]
  (let [from-left (mod col 3)
        from-top (mod row 3)]
    [(- col from-left) (- row from-top)]))

(defn block-values [board coord]
  (let [[row col] coord
        [block-row block-col] (coords-block row col)]
    (set
     (apply concat
      (for [r (range 0 3)]
       (for [c (range 0 3)]
         (value-at board [(+ block-row r) (+ block-col c)])))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     all-values
     (set/union
      (row-values board coord)
      (col-values board coord)
      (block-values board coord)))))

(defn filled? [board]
  (if (empty?
    (set/difference
     (apply set/union
      (for [row board]
        (set row)))
      all-values))
    true
    false))

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
