(require 'clojure.string)
(def input (clojure.string/split-lines (slurp "input10")))

(defn parse-line [l]
  (let [res (clojure.string/split l #" ")]
    (if (= 2 (count res))
      [(keyword (first res)) (read-string (second res))]
      [(keyword (first res))])))


;; Part 1
;; A bit overengineered but I feared extension of the instruction set for Part 2.

(def instrs (map parse-line input))
(defmulti to-cycle first)
(defmethod to-cycle :noop [_]
  [[:noop]])
(defmethod to-cycle :addx [instr]
  [[:noop] instr])

(def cycles (mapcat to-cycle instrs))

(defmulti interpret-aux (fn [[cmd] reg] cmd))
(defmethod interpret-aux :noop [_ reg]
  reg)
(defmethod interpret-aux :addx [[_ v] reg]
  (+ reg v))

(defn interpret [reg instr]
  (interpret-aux instr reg))

(defn get-val-at-point-fn [n]
  (fn [x] (* n (nth x (dec n)))))

(reduce + ((apply juxt (map get-val-at-point-fn [20 60 100 140  180 220]))
           (reductions interpret 1 cycles)))

;; Part 2

(defn interpret-with-sprite [[lit reg sprite] [instr param]]
  [(conj lit (if (<= (dec sprite) reg (inc sprite)) \# \.))
   (if (= instr :addx) (+ reg param) reg)
   (mod (inc sprite) 40)])

(doseq [x (partition 40 (first (reduce interpret-with-sprite [[] 1 0] cycles)))]
  (println x))
