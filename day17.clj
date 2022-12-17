(require 'clojure.string)

;; Part 1
(def stones [#{{:y 0, :x 2} {:y 0, :x 3} {:y 0, :x 4} {:y 0, :x 5}}
             #{{:y 1, :x 2} {:y 1, :x 3} {:y 1, :x 4} {:y 2, :x 3} {:y 0, :x 3}}
             #{{:y 0, :x 2} {:y 0, :x 3} {:y 0, :x 4} {:y 1, :x 4} {:y 2, :x 4}}
             #{{:y 0, :x 2} {:y 1, :x 2} {:y 2, :x 2} {:y 3, :x 2}}
             #{{:y 0, :x 2} {:y 1, :x 2} {:y 0, :x 3} {:y 1, :x 3}}])

(defn move [dir stone state]
  (let [newpos (set (map (case dir
                           \> (fn [m] (update m :x inc))
                           \< (fn [m] (update m :x dec))
                           \v (fn [m] (update m :y dec))) stone))]
    (if (and (every? (fn [{:keys [x]}] (<= 0 x 6)) newpos)
             (empty? (clojure.set/intersection state newpos)))
      newpos
      stone)))

(defn position-stone [stone max-y]
  (set (map (fn [m] (update m :y + max-y 4)) stone)))

(def a (atom {}))

(defn simulate-stones [n]
  (loop [cnt 0
         max-y 0
         pattern (cycle #_ ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" (clojure.string/trim (slurp "input17")))
         board #{{:x 0, :y 0} {:x 1, :y 0} {:x 2, :y 0} {:x 3, :y 0} {:x 4, :y 0} {:x 5, :y 0} {:x 6, :y 0}}
         cur-stone (position-stone (first stones) max-y)
         stones (rest (cycle stones))
         iterations 0]
    (if (= cnt n)
      max-y
      (let [gas-pos (move (first pattern) cur-stone board)
            new-stone-pos (move \v gas-pos board)]
        (if (= new-stone-pos gas-pos)
          (let [new-max-y (reduce max max-y (map :y new-stone-pos))]
            (when (zero? (mod cnt 5))
              (swap! a assoc new-stone-pos [(mod iterations 10091) iterations cnt])) ;; hacked for Part 2
            (recur (inc cnt) new-max-y (rest pattern)
                   (into board new-stone-pos) (position-stone (first stones) new-max-y) (rest stones) (inc iterations))) 
          (recur cnt max-y (rest pattern) board new-stone-pos stones (inc iterations)))))))

(simulate-stones 2022)

;; Part 2

;; get a good sample

(let [repeats-after (* (count stones) (count (clojure.string/trim (slurp "input17"))))]
  (simulate-stones (* repeats-after)))

;; inspect data manually
(->> @a
     (map (fn [[k v]] [(sort (map :x k)) v]))
     (group-by first)
     first
     second
     (map second)
     (sort-by second))

;; a bit of human interaction
;; [104 10195 1755]
;; [104 20286 3490]
;; [104 30377 5225]
;; after a prefix of 1755 stones, there seems to be a loop after 1735 stones

; (simulate-stones 1755) ; => 2800
; (simulate-stones 3490) ; => 5581
; (simulate-stones 5225) ; => 8362
;; adding 1735 stones then will increase the tower size by 2781

(+ (simulate-stones (+ 1755 (mod (- 1000000000000 1755) 1735))) (* (/ (- 1000000000000 1755 120) 1735) 2781))
;; 1602881844347
