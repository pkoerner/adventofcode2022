(require 'clojure.string)
(def lines (clojure.string/split-lines (slurp "input07")))

(defn parse-line [line]
  (remove nil? (rest (first (re-seq #"\$ (cd) (.+)$|\$ (ls)|(dir) (.*)|(\d+) (.*)" line)))))


;; Part 1

(def file-system 
  (loop [[[cmd arg] & tail :as lines] (map parse-line lines)
         path ()
         fs {}] 
    (if (empty? lines)
      fs  
      (case cmd 
        "cd" (if (= arg "..")
               (recur tail (rest path) fs) 
               (recur tail (conj path arg) fs))
        "ls" (recur tail path fs) 
        "dir" (recur tail path fs) 
        (recur tail path (update-in fs (reverse (conj path :files)) conj {:file arg, :size (read-string cmd)}))))))

(defn fs-size 
  ([m] (fs-size m []))
  ([m path] 
  (let [files (get m :files)
        dirs (dissoc m :files)
        subdir-tree (reduce merge 
                      (for [[dirname content] dirs]
                         (fs-size content (conj path dirname))))
        direct-subdirs (map #(get subdir-tree (conj path (first %))) dirs)]
    (assoc subdir-tree path (reduce + (concat (map :size files)
                                              direct-subdirs))))))

(def file-system-with-sizes (fs-size file-system))

(reduce + (filter (fn [x] (<= x 100000)) (vals file-system-with-sizes)))

;; Part 2

(let [total-disk-space 70000000
      size-req 30000000
      used-space (get file-system-with-sizes ["/"])
      free (- total-disk-space used-space)]
  (first (drop-while #(< % (- size-req free)) (sort (vals file-system-with-sizes )))))
