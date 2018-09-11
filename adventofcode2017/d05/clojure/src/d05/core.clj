(ns d05.core
  (:gen-class))

(def raw-input (slurp "../input.txt"))
(def input (->> raw-input
                clojure.string/trim
                clojure.string/split-lines
                (mapv #(Integer. %))))

(defn run [instrs modifier-fn]
  (let [instrs (transient instrs)]
    (loop [instrs instrs
           i 0
           n 0]
      (let [instr (get instrs i)]
        (if-not instr
          n
          (let [instrs (assoc! instrs i (modifier-fn instr))]
            (recur instrs (+ i instr) (inc n))))))))

(defn part-1 [instrs]
  (run instrs inc))

(defn part-2 [instrs]
  (run instrs (fn [n] (if (>= n 3) (dec n) (inc n)))))

(defn -main
  [& args]
  (println "part 1:" (part-1 input))
  (println "part 2:" (part-2 input)))
