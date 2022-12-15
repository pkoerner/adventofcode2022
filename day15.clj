(require 'clojure.string)
(def input (map (fn [line] (map read-string (re-seq #"-?\d+" line))) (clojure.string/split-lines (slurp "input15"))))

;; Part 1 
(defn abs' [x] (if (neg? x) (- x) x))

(defn manhattan [[x1 y1 x2 y2]]
  (+ (abs' (- x1 x2)) (abs' (- y1 y2))))

(defn blocks-at [level [sensor-x sensor-y beacon-x beacon-y]]
  (let [dist (manhattan [sensor-x sensor-y beacon-x beacon-y])
        y-dist (abs' (- level sensor-y))
        x-dist (- dist y-dist)]
    (for [x (range (- x-dist) (inc x-dist))]
      (+ x sensor-x))))

;; manually found a sensor at 2000000 in the input, removing that
(count (disj (set (mapcat (partial blocks-at 2000000) input)) 1690952))


;; Part 2

;; switch to intervals

(defn blocks-at [level [sensor-x sensor-y beacon-x beacon-y]]
  (let [dist (manhattan [sensor-x sensor-y beacon-x beacon-y])
        y-dist (abs' (- level sensor-y))
        x-dist (- dist y-dist)]
    (when (pos? x-dist)
      [(- sensor-x x-dist) (+ sensor-x x-dist 1)])))

(defn merge-intervals [is]
  (let [sorted (sort-by first (remove nil? is))]
    (reduce
      (fn [a [ilow ihi]]
        (let [[lo hi] (last a)]
          (cond (<= lo ilow ihi hi)
                a
                (<= lo ilow hi ihi)
                (conj (vec (butlast a)) [lo ihi])
                :otherwise
                (conj a [ilow ihi]))))
      [(first sorted)]
      (rest sorted))))

(defn covered-by? [[lo hi] [blo bhi]]
  (<= blo lo hi bhi))
(defn covered? [intervals i]
  (some (partial covered-by? i) intervals))

(def gridsize 4000000)
(def y-coord (first (for [y (range 0 (inc gridsize))
                          :when (not (covered? (merge-intervals (map (partial blocks-at y) input)) [0 gridsize]))]
                      y)))

(+ y-coord (* gridsize (get-in (merge-intervals (map (partial blocks-at y-coord) input)) [0 1])))
