(require 'clojure.string)
(def nums (map (comp (partial map read-string) (partial re-seq #"\d+")) (clojure.string/split-lines (slurp "input19"))))
(def input (map (fn [[nr ore clay obs-ore obs-clay geode-ore geode-obs]] {:blueprint-nr nr
                                                                          :ore-robot-ore-cost ore
                                                                          :clay-robot-ore-cost clay
                                                                          :obsidian-robot-ore-cost obs-ore
                                                                          :obsidian-robot-clay-cost obs-clay
                                                                          :geode-robot-ore-cost geode-ore
                                                                          :geode-robot-obsidian-cost geode-obs}) nums))

(defn build-ore-robot [{:keys [ore-robot-ore-cost] :as bp} {:keys [ore] :as state}]
  (when (<= ore-robot-ore-cost ore)
    (-> state
        (update :ore - ore-robot-ore-cost)
        (update :ore-robots inc))))

(defn build-clay-robot [{:keys [clay-robot-ore-cost]} {:keys [ore] :as state}]
  (when (<= clay-robot-ore-cost ore)
    (-> state
        (update :ore - clay-robot-ore-cost)
        (update :clay-robots inc))))

(defn build-obsidian-robot [{:keys [obsidian-robot-ore-cost obsidian-robot-clay-cost]} {:keys [ore clay] :as state}]
  (when (and (<= obsidian-robot-ore-cost ore)
             (<= obsidian-robot-clay-cost clay))
    (-> state
        (update :ore - obsidian-robot-ore-cost)
        (update :clay - obsidian-robot-clay-cost)
        (update :obsidian-robots inc))))

(defn build-geode-robot [{:keys [geode-robot-ore-cost geode-robot-obsidian-cost]} {:keys [ore obsidian] :as state}]
  (when (and (<= geode-robot-ore-cost ore)
             (<= geode-robot-obsidian-cost obsidian))
    (-> state
        (update :ore - geode-robot-ore-cost)
        (update :obsidian - geode-robot-obsidian-cost)
        (update :geode-robots inc))))

(defn simulate-blueprint [blueprint max-t]
  (loop [q [{:ore 0, :clay 0, :obsidian 0, :geode 0, :ore-robots 1, :clay-robots 0, :obsidian-robots 0, :geode-robots 0}]
         t 0]
    (if (>= t max-t)
      q
      (let [next-level (mapcat (fn [state]
                              (let [succs (cons state (keep (fn [f] (f blueprint state)) [build-ore-robot build-clay-robot build-obsidian-robot build-geode-robot])) ]
                                (map (fn [succ] (merge-with + succ {:ore (:ore-robots state)
                                                                    :clay (:clay-robots state)
                                                                    :obsidian (:obsidian-robots state)
                                                                    :geode (:geode-robots state)})) succs))) q)
            smaller (set (map (fn [state] (-> state
                                         (update :ore min 7)
                                         (update :clay min 22)
                                         (update :obsidian min 22)
                                         )) (remove (fn [state] (< 4 (:ore-robots state))) next-level))) 

            even-smaller (loop [next-level smaller
                                acc #{}]
                           (if (empty? next-level)
                             acc
                             (let [state (first next-level)]
                               (if (some (fn [state'] (and (<= (:ore state) (:ore state'))
                                                           (<= (:clay state) (:clay state'))
                                                           (<= (:obsidian state) (:obsidian state'))
                                                           (<= (:geode state) (:geode state'))
                                                           (<= (:ore-robots state) (:ore-robots state'))
                                                           (<= (:clay-robots state) (:clay-robots state'))
                                                           (<= (:obsidian-robots state) (:obsidian-robots state'))
                                                           (<= (:geode-robots state) (:geode-robots state')))) acc)            
                                 (recur (rest next-level) acc)
                                 (recur (rest next-level) (conj acc state))))))]
        (recur smaller (inc t))))))

;; Part 1
(def rr (for [blueprint input] [(:blueprint-nr blueprint) (reduce max (map :geode (simulate-blueprint blueprint 24)))]))
(doall rr)
(reduce + (map (partial apply *) (map (fn [r i] [(second r) (:blueprint-nr i)]) rr input)))

;; Part 2
(def rr2 (for [blueprint (take 3 input)] [(:blueprint-nr blueprint) (reduce max (map :geode (simulate-blueprint blueprint 32)))]))
(doall rr2)
(reduce * (map second rr2))
