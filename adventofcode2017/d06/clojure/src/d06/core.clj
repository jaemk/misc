(ns d06.core
  (:require [clojure.string :as string])
  (:gen-class))

(def input-raw (slurp "../input.txt"))
(def input (as-> input-raw v
                (string/trim v)
                (string/split v #"\s")
                (mapv #(Integer. %) v)))

(defn- find-greatest
  "from a seq of numbers, returns the left-most
  max value and its index
  numbers: seq of numbers
  return: [index max-value]"
  [numbers]
  (->> numbers
       (map vector (range))
       reverse
       (apply max-key #(get % 1))))

(defn- step
  "Distributes `value` among the set of banks, starting
  at `start-index, returning new banks"
  [banks start-index value]  ; [2 0 3 1], 2, 3
  (let [n (count banks)]
    (->> (repeat 1)
         (take value)                     ; [1 1 1]
         (concat (repeat start-index 0))  ; [0 0 1 1 1]
         (partition n n (repeat n 0))     ; [[0 0 1 1], [1 0 0 0]]
         (apply map +)                    ; [1 0 1 1]
         (mapv + banks))))                ; [3 0 4 2]

(defn solve [input]
  (loop [banks input
         seen {}
         n 0]
    (if (contains? seen banks)
      {:count n
       :diff (->> (seen banks) (- n))
       :banks banks}
      (let [seen (assoc seen banks n)
            [index value] (find-greatest banks)
            banks (assoc banks index 0)
            banks (step banks (inc index) value)]
        (recur banks seen (inc n))))))

(defn -main
  [& args]
  (let [{:keys [count diff]} (solve input)]
    (println "part 1:" count)
    (println "part 2:" diff)))
