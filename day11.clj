(require 'clojure.string)
(def raw (partition 7 7 nil (clojure.string/split-lines (slurp "input11"))))

(defn parse-monkey [[monkey-n items op test then else]]
  (let [n (first (rest (first (re-seq #"Monkey (\d+):" monkey-n))))
        items (str \[ (apply str (drop (count "  Starting Items:" ) items)) \])
        oplist (read-string (str \( (apply str (drop (count "  Operation: new = ") op)) \)))
        op `(fn [~'old] (~(second oplist) ~(first oplist) ~(nth oplist 2)))]
    {:monkey (read-string n)
     :items (read-string items)
     :op (eval op)
     :count 0
     :test-quot (read-string (apply str (drop (count "  Test: divisible by ") test)))
     :then (read-string (apply str (drop (count "    If true: throw to monkey ") then)))
     :else (read-string (apply str (drop (count "    If false: throw to monkey ") else)))}))

(defn monkey->mapitem [monkey]
  [(:monkey monkey) (dissoc monkey :monkey)])
(def monkeys (into {} (map (comp monkey->mapitem parse-monkey) raw)))

;; Part 1
(defn monkey-business [monkeys]
  (loop [monkeys monkeys
         current 0]
    (cond
      (> current (reduce max (keys monkeys))) monkeys
      (empty? (get-in monkeys [current :items])) (recur (assoc-in monkeys [current :items] []) (inc current))
      :otherwise
      (let [item (first (get-in monkeys [current :items]))
            newitem (quot ((get-in monkeys [current :op]) item) 3)]
        (recur
          (-> monkeys
              (update-in [current :items] rest)
              (update-in [current :count] inc)
              (update-in [(get-in monkeys [current (if (zero? (mod newitem (get-in monkeys [current :test-quot])))
                                                     :then :else)]) :items] conj newitem))
          current)))))

(apply * (map second (take 2 (sort-by (comp - second) (map (fn [[k v]] [k (:count v)]) (reduce (fn [a _] (monkey-business a)) monkeys (range 20)))))))


;; Part 2
(defn monkey-business [monkeys]
  (let [common-multiple (reduce * (map :test-quot (vals monkeys)))] 
    (loop [monkeys monkeys
           current 0]
      (cond
        (> current (reduce max (keys monkeys))) monkeys
        (empty? (get-in monkeys [current :items])) (recur (assoc-in monkeys [current :items] []) (inc current))
        :otherwise
        (let [item (first (get-in monkeys [current :items]))
              newitem (mod ((get-in monkeys [current :op]) item) common-multiple)
              ]
          (recur
            (-> monkeys
                (update-in [current :items] rest)
                (update-in [current :count] inc)
                (update-in [(get-in monkeys [current (if (zero? (mod newitem (get-in monkeys [current :test-quot])))
                                                       :then :else)]) :items] conj newitem))
            current))))))

(apply * (map second (take 2 (sort-by (comp - second) (map (fn [[k v]] [k (:count v)]) (reduce (fn [a _] (monkey-business a)) monkeys (range 10000)))))))
