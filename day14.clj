(require 'clojure.string)
(def input (map (fn [line] (map (fn [coords] (read-string (str \[ coords \]))) (clojure.string/split line #"->"))) (clojure.string/split-lines (slurp "input14"))))

(defn connect [[[x1 y1] [x2 y2]]]
  (cond (< x1 x2)
        (do (assert (= y1 y2))
            (map (fn [x] [x y1]) (range x1 (inc x2))))
        (< y1 y2)
        (do (assert (= x1 x2))
            (map (fn [y] [x1 y]) (range y1 (inc y2))) )
        (> x1 x2)
        (connect [[x2 y2] [x1 y1]])
        (> y1 y2)
        (connect [[x2 y2] [x1 y1]])
        :otherwise (throw (Exception. "panic"))))

(defn gen-path [line]
  (mapcat connect (partition 2 1 line)))

(def rocks (set (mapcat gen-path input)))
(def abyss (reduce max (map second rocks)))

(defn pour-sand [state]
  (loop [[x y] [500 0]]
    (cond 
      (> y abyss)
      (reduced state)
      (not (contains? state [x (inc y)]))
      (recur [x (inc y)])
      (not (contains? state [(dec x) (inc y)]))
      (recur [(dec x) (inc y)])
      (not (contains? state [(inc x) (inc y)]))
      (recur [(inc x) (inc y)])
      :otherwise
      (conj state [x y]))))

;; in the first step, nothing happens; in the last step, the sand does not settle
(- (count (reductions (fn [x _] (pour-sand x)) rocks (range 2000))) 2)


;; Part 2
(defn pour-sand [state]
  (loop [[x y] [500 0]]
    (cond 
      (= y (inc abyss))
      (conj state [x y])
      (not (contains? state [x (inc y)]))
      (recur [x (inc y)])
      (not (contains? state [(dec x) (inc y)]))
      (recur [(dec x) (inc y)])
      (not (contains? state [(inc x) (inc y)]))
      (recur [(inc x) (inc y)])
      :otherwise
      (conj state [x y]))))

(loop [state rocks 
       c 0]
  (if (contains? state [500 0])
    c
    (recur (pour-sand state) (inc c))))
