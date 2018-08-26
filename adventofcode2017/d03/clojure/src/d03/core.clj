(ns d03.core
  (:gen-class))

(def input 265149)


(defprotocol Mem
  "Memory thing"
  (init-mem [this] "makes new mem")
  (alloc-mem [this slots] "allocates space for n slots"))


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

(defn -step-mem [grid]
  (let [grid (update grid :size inc)
        mem (:mem grid)
        pos (:pos grid)
        dir (:dir grid)
        -update (fn [grid mem pos dir]
                  (assoc grid :mem mem :pos pos :dir dir))]
    (let [new-dir (-next-dir mem pos dir)
          new-pos (-step-dir pos new-dir)
          new-mem (conj mem new-pos)]
      (-update grid new-mem new-pos new-dir))))

(defrecord Grid []
  Mem
    (init-mem [this]
      (-> this
      (assoc :mem #{[0 0]})
      (assoc :size 1)
      (assoc :pos [0 0])
      (assoc :dir :up)))
    (alloc-mem [this slots]
      (loop [this this
             slots slots]
        (if (= (:size this) slots)
          this
          (recur (-step-mem this) slots)))))


(defn dist [[ax ay] [bx by]]
  (->> [(- bx ax) (- by ay)]
      (map #(Math/abs %))
      (apply +)))

(defn part-1 [n]
  (-> (Grid.)
      (init-mem)
      (alloc-mem n)
      ((fn [grid] (dist [0 0] (:pos grid))))))

(defn -main
  [& args]
  (let [p1 (part-1 input)]
    (println "Part 1:" p1)))
