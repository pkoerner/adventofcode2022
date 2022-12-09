(require 'clojure.string)
(def input (clojure.string/split-lines (slurp "input09")))

(defn parse-cmd [s] [(first s) (read-string (apply str (drop 2 s)))])

(def commands (map parse-cmd input))

;; Part 1

(defn abs [x]
  (if (> 0 x) (- x) x))

;; For the first part, I was able to cheat a bit on the function
;; that lets the tail catch up to the head.
;; I incorporated the direction instead of following the description.
;; Kicked me in the behind later on.
;; It shows once again that good habits on doing things the right way 
;; and taking no shortcuts really pay off.
(defn catch-up [[hx hy] [tx ty] dir]
  (if (and (<= (abs (- hx tx)) 1)
           (<= (abs (- hy ty)) 1))
    [tx ty]
    (case dir
      \U [hx (dec hy)]
      \D [hx (inc hy)]
      \L [(inc hx) hy]
      \R [(dec hx) hy])))

(defn step [[[hx hy] tail] dir]
  (let [head (case dir
               \U [hx (inc hy)]
               \D [hx (dec hy)]
               \R [(inc hx) hy]
               \L [(dec hx) hy])]
    [head (catch-up head tail dir)]))

(def more-commands (mapcat (fn [[c n]] (repeat n c)) commands))

(count (set (map second (reductions step [[0 0] [0 0]] more-commands))))


;; Part 2

(defn sign [x]
  (if (pos? x) 1 -1))

;; Proper function that lets the tail catch up to the head.
(defn catch-up [[hx hy] [tx ty]]
  (cond
    ;; touching
    (and (<= (abs (- hx tx)) 1)
         (<= (abs (- hy ty)) 1))
    [tx ty]
    ;; two steps same row
    (or (= hx tx) (= hy ty))
    [(+ tx (quot (- hx tx) 2))
     (+ ty (quot (- hy ty) 2))]
    :otherwise
    [(+ tx (sign (- hx tx)))
     (+ ty (sign (- hy ty)))]))


(defn catch-all-up [head tails]
  (loop [acc []
         head head
         tails tails]
    (if (empty? tails)
      acc 
      (recur (conj acc (catch-up head (first tails)))
             (catch-up head (first tails))
             (rest tails)))))

(defn step [[[hx hy] tails] dir]
  (let [head (case dir
               \U [hx (inc hy)]
               \D [hx (dec hy)]
               \R [(inc hx) hy]
               \L [(dec hx) hy])]
    [head (catch-all-up head tails)]))

(count (set (map (comp last second) (reductions step [[0 0] (repeat 9 [0 0])] more-commands))))  

;; helper for debugging - does not take into account negative indices
;; was used for replaying the examples
(defn visualise [[a b]]
  (let [m (into {} (reverse (map-indexed (fn [idx v] [v idx]) (cons a b))))
        height (reduce max (map second (cons a b )))
        width (reduce max (map first (cons a b )))]
    (doseq [y (reverse (range (inc height)))]
      (doseq [x (range (inc width))]
        (print (get m [x y] \space)))
      (println)))
  (println))

;; example call (though much happens in the negative area, so this particular one is useless):
(comment 
  (doseq [x (reductions step [[0 0] (repeat 9 [0 0])] (take 10 more-commands))]
    (visualise x)))  
