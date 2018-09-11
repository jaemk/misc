(ns d03.core
  (:gen-class))

(def input 265149)


(defprotocol Mem
  "Memory thing"
  (init-mem [this] "makes new mem")
  (alloc-mem [this slots insert-val] "allocate space for n slots inserting a value using insert-val"))


(defn -turn-left [dir]
  (cond
    (= dir :up) :left
    (= dir :left) :down
    (= dir :down) :right
    (= dir :right) :up))

(defn -pos-left [[x y] dir]
  (cond
    (= dir :up) [(dec x) y]
    (= dir :down) [(inc x) y]
    (= dir :left) [x (dec y)]
    (= dir :right) [x (inc y)]))


(defn -next-dir [mem pos dir]
  (let [left-pos (-pos-left pos dir)]
    (if (contains? mem left-pos)
      dir
      (-turn-left dir))))


(defn -step-dir [[x y] dir]
  (cond
    (= dir :up) [x (inc y)]
    (= dir :left) [(dec x) y]
    (= dir :down) [x (dec y)]
    (= dir :right) [(inc x) y]))

(defn -step-mem [grid calc-val]
  (let [grid (update grid :size inc)
        mem (:mem grid)
        pos (:pos grid)
        dir (:dir grid)
        -update (fn [grid mem pos dir]
                  (assoc grid :mem mem :pos pos :dir dir))]
    (let [new-dir (-next-dir mem pos dir)
          new-pos (-step-dir pos new-dir)
          new-val (calc-val mem new-pos)
          new-mem (assoc mem new-pos new-val)]
      (-update grid new-mem new-pos new-dir))))

(defrecord Grid []
  Mem
    (init-mem [this]
      (-> this
      (assoc :mem {[0 0] 1})
      (assoc :size 1)
      (assoc :pos [0 0])
      (assoc :dir :up)))
    (alloc-mem [this slots insert-val]
      (loop [this this
             slots slots]
        (if (>= (:size this) slots)
          this
          (recur (-step-mem this insert-val) slots)))))

(defn alloc-grid-mem [grid slots & {:keys [calc-val] :or {calc-val (fn [& args] 1)}}]
  (alloc-mem grid slots calc-val))

(defn dist [[ax ay] [bx by]]
  (->> [(- bx ax) (- by ay)]
      (map #(Math/abs %))
      (apply +)))

(defn part-1 [n]
  (-> (Grid.)
      (init-mem)
      (alloc-grid-mem n)
      ((fn [grid] (dist [0 0] (:pos grid))))))

(defn -grid-value [grid]
  ((:mem grid) (:pos grid)))

(defn -surrounding-cells [[x y]]
  [[(inc x) y]
   [(inc x) (inc y)]
   [x (inc y)]
   [(dec x) (inc y)]
   [(dec x) y]
   [(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]])

(defn -sum-surrounding [mem new-pos]
  (let [cells (-surrounding-cells new-pos)]
    (->> cells
         (map #(get mem % 0))
         (apply +))))

(defn part-2 [n]
  (as-> (Grid.) g
    (init-mem g)
    (loop [g g
           size 1]
      (let [grid-val (-grid-value g)]
        (if (> grid-val n)
          grid-val
          (recur (alloc-grid-mem g size :calc-val -sum-surrounding) (inc size)))))))

(defn -main
  [& args]
  (do
    (print "Part 1: ")
    (println (part-1 input)))
  (do
    (print "Part 2:")
    (println (part-2 input))))
