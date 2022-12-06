(def input (slurp "input06"))

;; Part 1

(let [n 4]
  (+ n (count (take-while false? (map (partial apply distinct?) (partition n 1 input))))))

;; Part 2

(let [n 14]
  (+ n (count (take-while false? (map (partial apply distinct?) (partition n 1 input))))))

