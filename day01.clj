(require 'clojure.string)
(def clean-input (map (partial map read-string) (remove #(clojure.string/blank? (first %)) (partition-by empty? (clojure.string/split-lines (slurp "input01"))))))

;; Part 1
(reduce max (map (partial reduce +) clean-input))

;; Part 2
(apply + (take 3 (sort-by - (map (partial reduce +) clean-input))))
