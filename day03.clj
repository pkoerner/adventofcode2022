(require 'clojure.string)
(require 'clojure.set)
(def input (clojure.string/split-lines (slurp "input03")))

;; Part 1
(def items (mapcat (partial apply clojure.set/intersection) (map (fn [[l r]] [(set l) (set r)]) (map (fn [x] (split-at (/ (count x) 2) x)) input))))

(defn char->priority [x]
  (inc (- (int x)
          (if (Character/isUpperCase x)
            (- (int \A) 26)
            (int \a)))))

(reduce + (map char->priority items))

;; Part 2
(reduce + (map char->priority (mapcat (partial apply clojure.set/intersection) (map (partial map set) (partition 3 input)))))
