(require 'clojure.string)
(def input (clojure.string/split-lines (slurp "input22")))
(def world (take 200 input))
(def command-str (last input))

(def world-map (into {} (for [y (range (count world))
                              :let [line (nth world y)]
                              x (range (count line))
                              :let [c (nth line x)]
                              :when (not= c \space) ]
                          [[(inc x) (inc y)] c])))

(def commands (let [part (partition-by #{\L \R} command-str)]
                (mapcat (fn [x]
                          (case x
                            [\L] [:left]
                            [\R] [:right]
                            (repeat (read-string (apply str x)) :forward))) part)))


;; Part 1
(defn move [[[x y :as rover-pos] rover-dir] cmd]
  (case cmd
    :left  [rover-pos ({:south :east, :east :north, :north :west, :west :south} rover-dir)]
    :right [rover-pos ({:south :west, :west :north, :north :east, :east :south} rover-dir)]
    :forward
    (let [target (case rover-dir
                   :north [x (dec y)]
                   :south [x (inc y)]
                   :west  [(dec x) y]
                   :east  [(inc x) y])
          new-target (if (get world-map target)
                        target
                        ;; wrap-around
                        (loop [[x y] rover-pos]
                          (let [coords (case rover-dir
                                      :north [x (inc y)]
                                      :south [x (dec y)]
                                      :west  [(inc x) y]
                                      :east  [(dec x) y])]
                            (if (get world-map coords)
                              (recur coords)
                              [x y]))))]
     (case (get world-map new-target)
       \. [new-target rover-dir]
       \# [rover-pos rover-dir]))))

(let [[[column row] orientation] (reduce move [[51 1] :east] commands)]
  (+ (* 1000 row) (* 4 column) ({:west 2, :east 0, :south 1, :north 3} orientation)))

;; Part 2
(defn move [[[x y :as rover-pos] rover-dir] cmd]
  (case cmd
    :left  [rover-pos ({:south :east, :east :north, :north :west, :west :south} rover-dir)]
    :right [rover-pos ({:south :west, :west :north, :north :east, :east :south} rover-dir)]
    :forward
    (let [[to-x to-y :as target] (case rover-dir
                                   :north [x (dec y)]
                                   :south [x (inc y)]
                                   :west  [(dec x) y]
                                   :east  [(inc x) y])
          [new-target new-dir] (if (get world-map target)
                                 [target rover-dir]
                                 ;; wrap-around
                                 (cond (and (= to-x 50)       (<=   1 to-y  50) (= :west rover-dir)) ;; edge 1
                                       [[1 (- 151 y)] :east]
                                       (and (<=  51 to-x 100) (= to-y 0)        (= :north rover-dir)) ;; edge 2
                                       [[1 (+ 100 x)] :east]
                                       (and (<= 101 to-x 150) (= to-y 0)        (= :north rover-dir)) ;; edge 3
                                       [[(- x 100) 200] :north]
                                       (and (= to-x 151)      (<=   1 to-y  50) (= :east rover-dir))  ;; edge 4
                                       [[100 (- 151 y)] :west]
                                       (and (<= 101 to-x 150) (= to-y 51)       (= :south rover-dir)) ;; edge 5
                                       [[100 (- x 50)] :west]
                                       (and (= to-x 101)      (<=  51 to-y 100) (= :east rover-dir))  ;; edge 6
                                       [[(+ y 50) 50] :north]
                                       (and (= to-x 101)      (<= 101 to-y 150) (= :east rover-dir))  ;; edge 7
                                       [[150 (- 151 y)] :west]
                                       (and (<= 51 to-x 100)  (= to-y 151)      (= :south rover-dir)) ;; edge 8
                                       [[50 (+ x 100)] :west]
                                       (and (= to-x 51)       (<= 151 to-y 200) (= :east rover-dir))  ;; edge 9
                                       [[(- y 100) 150] :north]
                                       (and (<=  1 to-x  50)  (= to-y 201)      (= :south rover-dir)) ;; edge 10
                                       [[(+ 100 x) 1] :south]
                                       (and (= to-x 0)        (<= 151 to-y 200) (= :west rover-dir))  ;; edge 11
                                       [[(- y 100) 1] :south]
                                       (and (= to-x 0)        (<= 101 to-y 150) (= :west rover-dir))  ;; edge 12
                                       [[51 (- 151 y)] :east]
                                       (and (<=   1 to-x  50) (= to-y 100)      (= :north rover-dir)) ;; edge 13
                                       [[51 (+ 50 x)] :east]
                                       (and (= to-x 50)       (<=  51 to-y 100) (= :west rover-dir))  ;; edge 12
                                       [[(- y 50) 101] :south]))]
     (case (get world-map new-target)
       \. [new-target new-dir]
       \# [rover-pos rover-dir]))))

(let [[[column row] orientation] (reduce move [[51 1] :east] commands)]
  (+ (* 1000 row) (* 4 column) ({:west 2, :east 0, :south 1, :north 3} orientation)))
