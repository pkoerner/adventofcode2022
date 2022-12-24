(require 'clojure.string)
(def lines (clojure.string/split-lines (slurp "input24")))
(def blizzards (into #{} (for [y (range (count lines))
                               x (range (count (first lines)))
                               :let [c (nth (nth lines y) x)]
                               :when (#{\v \< \> \^} c)]
                           [[x y] c])))

(defn up    [[x y]] [x (dec y)])
(defn down  [[x y]] [x (inc y)])
(defn left  [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])

(defn move-blizzard [[[x y] direction]]
  (case direction
    \< (if (= x 1)   [[120 y] direction] [(left  [x y]) direction])
    \> (if (= x 120)   [[1 y] direction] [(right [x y]) direction])
    \^ (if (= y 1)    [[x 25] direction] [(up    [x y]) direction])
    \v (if (= y 25)    [[x 1] direction] [(down  [x y]) direction])))

(defn move-blizzards [blizzards]
  (into #{} (map move-blizzard blizzards)))

(defn sol [init blizzards goal-minus-1]
  (loop [q #{init}
         n 1
         blizzards blizzards]
    (when (> n 10000) (println :oops) (throw (Exception. (str [(count q)]))))
    (if (q goal-minus-1)
      [n blizzards]
      (let [next-blizzards (move-blizzards blizzards)
            blizzard-poss (set (map first next-blizzards))
            succs (set (mapcat (fn [pos] (remove blizzard-poss (filter (fn [[x y]] (or (= [x y] init) (and (< 0 x 121) (< 0 y 26)))) ((juxt identity up down left right) pos)))) q))]
        (recur succs (inc n) next-blizzards)))))

;; Part 1
(first (sol [1 0] blizzards [120 25]))

;; Part 2
(let [[n1 blizz1] (sol [1 0] blizzards [120 25])
      [n2 blizz2] (sol [120 26] (move-blizzards blizz1) [1 1])
      [n3 _]      (sol [1 0] (move-blizzards blizz2) [120 25])]
  (+ n1 n2 n3))
