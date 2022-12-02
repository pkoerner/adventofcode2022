(def input (partition 2 (read-string (str \[ (slurp "input02") \]))))

;; Part 1
(def sym->shape {'A :rock, 'B :paper, 'C :scissors,
                 'X :rock, 'Y :paper, 'Z :scissors})
(def shape->score {:rock 1, :paper 2, :scissors 3})
(def outcome {[:rock :rock] :draw,
              [:rock :paper] :win
              [:rock :scissors] :loss
              [:paper :rock] :loss
              [:paper :paper] :draw
              [:paper :scissors] :win
              [:scissors :rock] :win
              [:scissors :paper] :loss
              [:scissors :scissors] :draw})
(def outcome->score {:loss 0, :draw 3, :win 6})

(defn scoring [rounds]
  (reduce + (map (fn [[opponent me]] 
                   (let [opponent (sym->shape opponent)
                         me (sym->shape me)]
                     (+ (shape->score me)
                        (outcome->score (outcome [opponent me])))))
                 rounds)))

(scoring input)

;; Part 2
(def sym->entity {'A :rock, 'B :paper, 'C :scissors,
                  'X :loss, 'Y :draw, 'Z :win})
(def required-shape (into {} (map (fn [[[me opponent] res]] [[me res] opponent]) outcome)))

(defn scoring2 [rounds]
  (reduce + (map (fn [[opponent res]] 
                   (let [opponent (sym->shape opponent)
                         res (sym->entity res)]
                     (+ (shape->score (required-shape [opponent res]))
                        (outcome->score res))))
                 rounds)))

(scoring2 input)
