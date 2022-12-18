(require 'clojure.string)
(def input (map (fn [line] (read-string (str \[ line \]))) (clojure.string/split-lines (slurp "input18"))))

;; Part 1
(defn neighbours [[x y z]]
  [[(inc x) y z]
   [(dec x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (inc z)]
   [x y (dec z)]])

(defn calculate-surface-area [input]
  (loop [blocks #{}
         surface 0
         [b & t :as q] input]
    (if (empty? q)
      surface
      (let [neigh (neighbours b)
            n-adjacent (count (filter blocks neigh))]
        (recur (conj blocks b) (+ surface (- 6 (* 2 n-adjacent))) t)))))

(calculate-surface-area input)

;; Part 2

(def air (set (let [input (set input)]
                (for [x (range 21)
                      y (range 21)
                      z (range 21)
                      :let [b [x y z]]
                      :when (not (input b))]
                  b))))


(def reachable-air (let [input (set input)]
                     (loop [seen #{[0 0 0]}
                            q [[0 0 0]]
                            n 0]
                       (if (empty? q)
                         seen
                         (let [next-states (neighbours (first q))
                               in-bounds (filter (fn [coords] (every? (fn [dim] (<= 0 dim 21)) coords)) next-states)
                               to-enq (remove input (remove seen in-bounds))]
                           (recur (into seen to-enq) (into (rest q) to-enq) (inc n)))))))


(- 3636 ;; entire surface area
   (calculate-surface-area (clojure.set/difference air reachable-air)))
