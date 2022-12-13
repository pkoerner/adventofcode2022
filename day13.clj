(def input (partition 2 (read-string (str \[ (slurp "input13") \])))) 

;; Note: it took me a while to realise I needed to distunguish equality separately 
;; (e.g., using something three-valued like :lt, :eq, :gt or -1, 0, 1 in typical comparators)
;; rather than just being able to work with <= and >.
;; The code can probably be simplified after re-working this.

;; Part 1

(defn ordered? [l r]
  (cond
    (and (integer? l) (integer? r))
    (cond (< l r) :lt
          (= l r) :eq
          :otherwise :gt) 
    (and (coll? l) (integer? r))
    (ordered? l [r])
    (and (integer? l) (coll? r))
    (ordered? [l] r)
    :otherwise
    (do (assert (and (coll? l) (coll? r)))
        (loop [[lh & lt :as l] l
               [rh & rt :as r] r]
          (cond
            (and (seq l) (seq r))
            (cond (= (ordered? lh rh) :eq)
                  (recur lt rt)
                  :otherwise
                  (ordered? lh rh))
            (seq l)
            :gt
            (seq r)
            :lt
            :otherwise
            :eq)))))

(reduce + (map-indexed (fn [idx b] (* (inc idx) ({:lt 1, :gt 0} b))) (map (partial apply ordered?) input)))

;; Part 2

(let [m (into {} (map-indexed (comp vec reverse vector) (sort (fn [l r] ({:lt -1, :gt 1, :eq 0} (ordered? l r))) (apply concat [[[2]]] [[[6]]] input))))]
  (* (inc (get m [[2]]))
     (inc (get m [[6]]))))
