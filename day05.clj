(require 'clojure.string)
(def lines (clojure.string/split-lines (slurp "input05")))
(def boxes' (map (fn [line] (map second (partition 4 4 nil line))) (take 8 lines)))
(def boxes (vec (apply map (comp (partial drop-while #{\space}) vector) boxes')))

(def instrs (map (fn [line] (map read-string (rest (first (re-seq #"move (\d+) from (\d+) to (\d+)" line))))) (drop 10 lines)))

;; Part 1

(defn execute-instr [boxes [n from to]]
  (let [to-move (take n (get boxes (dec from)))]
    (-> boxes
        (update (dec from) (fn [x] (drop n x)))
        (update (dec to) (fn [x] (concat (reverse to-move) x))))))

(apply str (map first (reduce execute-instr boxes instrs)))

;; Part 2

(defn execute-instr' [boxes [n from to]]
  (let [to-move (take n (get boxes (dec from)))]
    (-> boxes
        (update (dec from) (fn [x] (drop n x)))
        (update (dec to) (fn [x] (concat to-move x))))))

(apply str (map first (reduce execute-instr' boxes instrs)))
