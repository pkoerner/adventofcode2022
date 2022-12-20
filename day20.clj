(def input (read-string (str \[ (slurp "input20") \])))
(def c (count input))

(defn sequenzise [v] (map-indexed (fn [idx v] {:turn idx :val v}) v))
(def sequenze (sequenzise input))

(defn do-step [array item]
  (let [pos (.indexOf array item)
        move (mod (:val item) (dec c))]
    (cond (< (+ pos move) c)
          (doall (concat (take pos array) (take move (drop (inc pos) array)) [item] (drop (+ pos move 1) array)))
          :otherwise
          (let [wrap-around (- move (- (dec c) pos))]
            (doall (concat (take wrap-around array) [item] (take (- pos wrap-around) (drop wrap-around array)) (drop (inc pos) array)))))))

;; Part 1
(def result (map :val (reduce do-step sequenze sequenze)))
; (.indexOf result 0)
(+ (nth result 1000) (nth result 2000) (nth result 3000))


;; Part 2
(def sequenze (sequenzise (map (partial * 811589153) input)))
(def result (map :val (reduce do-step sequenze (apply concat (repeat 10 sequenze)))))
; (.indexOf result 0)
(+ (nth result 1000) (nth result 2000) (nth result 3000))
