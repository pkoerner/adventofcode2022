(require 'clojure.string)
(def input (clojure.string/split-lines (slurp "input23")))
(def elves (into #{} (for [y (range (count input))
                           x (range (count (first input)))
                           :when (= \# (nth (nth input y) x))]
                       {:x x, :y y})))

(defn move-proposal [elves elf round]
  (let [N (update elf :y dec)
        NE (-> elf (update :y dec) (update :x inc))
        NW (-> elf (update :y dec) (update :x dec))
        W (-> elf (update :x dec))
        E (-> elf (update :x inc))
        S (update elf :y inc)
        SE (-> elf (update :y inc) (update :x inc))
        SW (-> elf (update :y inc) (update :x dec))]
    (let [ordering (drop (mod round 4) (cycle [[N NE NW] [S SE SW] [W NW SW] [E NE SE]]))
          directions (drop (mod round 4) (cycle [N S W E]))]
    (cond (every? (comp nil? elves) [N NE NW W E S SE SW])
          [elf elf]
          (every? (comp nil? elves) (nth ordering 0))
          [elf (nth directions 0)]
          (every? (comp nil? elves) (nth ordering 1))
          [elf (nth directions 1)]
          (every? (comp nil? elves) (nth ordering 2))
          [elf (nth directions 2)]
          (every? (comp nil? elves) (nth ordering 3))
          [elf (nth directions 3)]
          :otherwise [elf elf] ;; ?
          ))))

(defn move-elves [elves round]
  (let [proposals (into {} (for [elf elves]
                             (move-proposal elves elf round)))
        targets (vals proposals)
        dupes (set (map first (filter (fn [[k v]] (< 1 v)) (frequencies targets))))]
    (into #{} (map (fn [[from to]] (if (dupes to) from to)) proposals))))

;; Part 1
(let [poss (reduce move-elves elves (range 10))
      xs (map :x poss)
      ys (map :y poss)
      xmin (reduce min xs)
      xmax (reduce max xs)
      ymin (reduce min ys)
      ymax (reduce max ys)]
  (count (for [x (range xmin (inc xmax))
        y (range ymin (inc ymax))
        :let [pos {:x x, :y y}]
        :when (nil? (get elves pos))]
    {:x x :y y})))

;; Part 2
(loop [poss elves
       round 0]
  (let [next-poss (move-elves poss round)]
    (if (= next-poss poss)
      (inc round)
      (recur next-poss (inc round)))))
