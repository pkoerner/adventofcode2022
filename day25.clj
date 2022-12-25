(require 'clojure.string)
(def snafus (clojure.string/split-lines (slurp "input25")))
((first snafus))
(defn snafu->dec [s]
  (loop [s (reverse s)
         factor 1
         acc 0]
    (if (empty? s)
      acc
      (recur (rest s)
             (* 5 factor)
             (+ acc
                (case (first s)
                  \0 0
                  \1 factor
                  \2 (* 2 factor)
                  \- (- factor)
                  \= (* 2 (- factor))))))))

(reduce + (map snafu->dec snafus))

(defn dec->snafu [n]
  (loop [n n
         acc []]
    (if (zero? n)
      (reverse acc)
      (recur
        (if (<= (mod n 5) 2)
          (quot n 5)
          (inc (quot n 5)))
        (conj acc (case (mod n 5)
                    0 \0
                    1 \1
                    2 \2
                    3 \=
                    4 \-))))))

(apply str (dec->snafu (reduce + (map snafu->dec snafus))))
