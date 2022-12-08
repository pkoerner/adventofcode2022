(require 'clojure.string)
(def input (vec (map (comp vec (partial map int)) (clojure.string/split-lines (slurp "input08")))))


;; Quite unhappy with the solution today:
;; Idea: Extract a row from the grid, check to the left and right of a position.
;; For the north/south direction, transpose the entire matrix.
;; Lots of calculation is duplicated and this is not efficient.
;; However, this solution is still within reasonable time (< 1 sec) for the given input size.

;; Part 1

(defn all-smaller? [c e]
  (every? (fn [x] (< x e)) c))

(defn visible-from-hor [input x y]
  (let [row (get input y)
        west (take x row)
        east (drop (inc x) row) ]
    (or (all-smaller? west (get row x))
        (all-smaller? east (get row x)))))

((frequencies (let [input' (vec (apply map vector input))
                    width (count (first input))
                    height (count input)]
                (for [x (range width)
                      y (range height)]
                  (or (visible-from-hor input x y)
                      (visible-from-hor input' y x)
                      )))) true)

;; Part 2

(defn viewable-hor [input x y]
  (let [row (get input y)
        west (reverse (take x row))
        east (drop (inc x) row)
        c1 (count (take-while #(< % (get row x)) west))
        c2 (count (take-while #(< % (get row x)) east)) ]
    (* (if (= c1 (count west)) c1 (inc c1))
       (if (= c2 (count east)) c2 (inc c2)))))

(reduce max (let [input' (vec (apply map vector input))
      width (count (first input))
      height (count input)]
  (for [x (range width)
        y (range height)]
    (* (viewable-hor input x y)
       (viewable-hor input' y x)))))
