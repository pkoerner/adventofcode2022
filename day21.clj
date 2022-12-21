(require 'clojure.string)
(def input (clojure.string/split-lines (slurp "input21")))
(def mappo (into {} (map (fn [line] (if (< (count line) 11)
                                      (let [[nam numstr] (re-seq #"[a-z]+|\d+" line)] [nam (read-string numstr)])
                                      (let [[nam op1 op op2] (re-seq #"[a-z]+|\+|-|\*|/" line)] [nam [op1 op op2]]))) input)))

(defn calc [target]
  (if (integer? (get mappo target))
    (get mappo target)
    (let [[op1 op op2] (get mappo target)]
      (({"+" +, "-" -, "*" *, "/" /} op) (calc op1) (calc op2)))))

(calc "root")

(defn calc [target n]
  (if (= target "root")
    (let [[op1 _ op2] (get mappo target)]
      (= (calc op1 n) (calc op2 n)))
    (if (= target "humn") n
      (if (integer? (get mappo target))
        (get mappo target)
        (let [[op1 op op2] (get mappo target)]
          (({"+" +, "-" -, "*" *, "/" /} op) (calc op1 n) (calc op2 n)))))))

(defn bin-search [lo hi]
  (let [mid (long (/ (+ hi lo) 2))
        v (calc "lsbv" mid)]
    (cond
      (= 2228768553328 v)
      mid
      (= lo hi)
        lo
      (= (inc lo) hi)
        hi
      (< v 2228768553328)
        (recur lo mid)
      :otherwise
        (recur mid hi))))

(bin-search 0 5000000000000)
