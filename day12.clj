(require 'clojure.string)
(def input (into {} (apply concat (map-indexed (fn [y line] (map-indexed (fn [x c] [{:x x, :y y} c]) line)) (clojure.string/split-lines (slurp "input12"))))))
(def specials (into {} (for [[k v] input :when (#{\S \E} v)] [v k])))
(def input' (assoc input (get specials \S) \a
                         (get specials \E) \z))

;; newer versions of Clojure seems to have this as a built-in
(defn abs [x] (if (neg? x) (- x) x))

(defn neighbours [{:keys [x y] :as orig}]
  (filter (fn [x] (and (contains? input' x)
                       (<= (int (get input' x)) (inc (int (get input' orig))))))
          [{:x (dec x), :y y}
           {:x (inc x), :y y}
           {:x x, :y (inc y)}
           {:x x, :y (dec y)}]))

(defn bfs [start goal]
  (loop [open start
         visited #{}
         step 0]
    (when (< step (count input))
      (if (contains? (set open) goal)
        step
        (let [neighbours (set (mapcat neighbours open))]
          (recur (remove visited neighbours) (into visited open) (inc step)))))))

;; Part 1
(bfs [(get specials \S)] (get specials \E))

;; Part 2
(bfs (keys (filter (fn [[k v]] (= v \a)) input')) (get specials \E))
