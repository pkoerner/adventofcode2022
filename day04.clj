(require 'clojure.string)
(def nums (map (fn [line] (map read-string (rest (first (re-seq #"(\d*)-(\d*),(\d*)-(\d*)" line))))) (clojure.string/split-lines (slurp "input04"))))

;; Part 1

(defn subsumes? [[lo1 hi1, lo2 hi2]]
  (or (<= lo1 lo2 hi2 hi1)
      (<= lo2 lo1 hi1 hi2)))

(count (filter subsumes? nums))

;; Part 2

(defn overlaps? [[lo1 hi1, lo2 hi2]]
  (not (or (< hi1 lo2)
           (< hi2 lo1))))

(count (filter overlaps? nums))
