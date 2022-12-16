(require 'clojure.string)
(def partial-parse (map (fn [line] (rest (first (re-seq #"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)$" line)))) (clojure.string/split-lines (slurp "input16"))))
(def input (into {} (map (fn [[valve flowstr valves]]
                           [valve {:flow (read-string flowstr)
                                   :connections (map clojure.string/trim (clojure.string/split valves #","))}]) partial-parse)))

;; Part 1

(defn bfs [start goal]
  (loop [depth 0
         poss #{start}]
    (if (contains? poss goal)
      depth
      (recur (inc depth) (set (mapcat (fn [pos] (:connections (get input pos))) poss))))))

(def valves (map key (filter (fn [[k v]] (pos? (:flow v))) input)))
(def distance (into {} (for [location (conj valves "AA")
                             valve valves]
                         [[location valve] (bfs location valve)])))

;; can't be bothered to clean this up
(defn generate-sequences [input]
  (loop [q [["AA" 0 [] #{}]]
         acc []
         depth 0
         seen #{}
         ]
    (if (empty? q)
      acc
      (let [[pos t path visited-valves] (first q)]
        (if (seen (first q))
          (recur (rest q) acc (inc depth) seen)
          (if (<= 30 t)
            (recur (rest q) (conj acc path) (inc depth) (conj seen [pos t path visited-valves]))
            (let [closed-valves (clojure.set/difference (set valves) visited-valves)
                  next-states (map (fn [next-valve] [next-valve (+ t (get distance [pos next-valve])) path visited-valves]) (disj closed-valves pos))
                  open-valve [pos (inc t) (conj path [:open pos :with-flow (get-in input [pos :flow]) :at (inc t)]) (conj visited-valves pos)]]
              #_(println :from (first q) :to next-states :or open-valve)
              (recur (into (rest q)
                           (if (get closed-valves pos) [open-valve] 
                             next-states
                             ))
                     acc (inc depth)
                     (conj seen [pos t path visited-valves])
                     ))))))))


(def seqs (set (generate-sequences input)))

(defn handle-seq-element [[_ _ _ flow _ t]]
  (assert (<= t 30))
  (* (- 30 t) flow))

(first (sort-by - (map (fn [s] (reduce + (map handle-seq-element s))) seqs)))

;; Part 2
(def seqs-b (set (map (fn [s] (remove (fn [x] (< 26 (last x))) s)) seqs)))


(defn handle-seq-element' [[_ _ _ flow _ t]]
  (assert (<= t 26))
  (* (- 26 t) flow))

(defn scoring [s] (reduce + (map handle-seq-element' s)))
(def topscores (sort-by (comp - scoring) seqs-b))


(def disj-top (for [a (take 500 topscores) ;; 500 is picked somewhat arbitrarily. The first match did not provide the correct answer.
                    b (take 500 topscores)
                    :let [a-valves (set (map second a))
                          b-valves (set (map second b))]
                    :when (empty? (clojure.set/intersection a-valves b-valves))]
                [a b]))

(first (sort-by - (map (fn [[a b]] (+ (scoring a) (scoring b))) disj-top)))
