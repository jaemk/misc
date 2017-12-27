(ns fingercount.core)

;; L-left, R-right
;; P-pinky, R-Ring, M-middle, I-index, T-thumb
;; LP LR LM LI LT RT RI RM RR RP
;; 0  1  1  1  0  1  1  1  0  0
;; left hands fingers are 10's, right are 1's,
;; thumbs are 5*(the hand's value)

(def fvals [10 10 10 10 50 5 1 1 1 1 1])

(defn ->int [c]
  (Integer. (str c)))

(defn valid?
  "raised fingers must be consecutive"
  [bs]
  (let [[left right] (split-at 5 bs)
        right (map ->int (rest right))
        left (->> left reverse rest (map ->int))]
    (every? true? (map #(apply >= %) [left right]))))

(defn finger
  "finger counter"
  [bin-string]
  (if (valid? bin-string)
    (->> bin-string
         (map-indexed #(if (= \1 %2) (fvals %1) 0))
         (apply +))
    :invalid))
