(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

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
  (into [] (for [row coords
        col coords]
    [row col])))

(defn coords-block [row col]
  (let [from-left (mod col 3)
        from-top (mod row 3)]
    [(- row from-top) (- col from-left)]))

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
  (reduce #(conj %1 (set %2)) [] board))

(defn valid-rows? [board]
  (= (first (distinct (rows board))) all-values))

(defn cols [board]
  (reduce #(conj %1 (col-values board [0 %2])) [] (range 0 9)))

(defn valid-cols? [board]
  (apply = (conj (distinct (cols board)) all-values)))


(defn blocks [board]
  (into [] (for [col [0 3 6]
        row [0 3 6]]
    (block-values board [row col]))))


(defn valid-blocks? [board]
  (= (first (distinct (rows board))) all-values))


(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))


(defn set-value-at [board coord new-value]
  (let [[row col] coord]
    (assoc-in board coord new-value)))

(defn find-empty-point-from-row [row]
  (loop [column-idx 0]
    (cond
     (= column-idx (count row)) (- 1)
     (zero? (get row column-idx)) column-idx
     :else (recur (inc column-idx)))))

(defn find-empty-point [board]
  (loop [row-idx 0
         col-idx (find-empty-point-from-row (get board 0))]
    (cond
     (not (neg? col-idx)) [row-idx col-idx]
     (= row-idx (count board)) []
     :else (recur (inc row-idx) (find-empty-point-from-row (get board (inc row-idx)))))))

(defn solve-helper [board]
  (let [empty-coord (find-empty-point board)]
    (cond
     (filled? board) board
     :else (flatten (map #(solve-helper (set-value-at board empty-coord %))
                         (valid-values-for board empty-coord))))))

(defn solve [board]
  (partition 9 (solve-helper board)))

