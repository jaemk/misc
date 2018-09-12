(ns d06.core
  (:require [clojure.string :as string])
  (:gen-class))

(def input-raw (slurp "../input.txt"))
(def input (as-> input-raw v
                (string/trim v)
                (string/split v #"\s")
                (mapv #(Integer. %) v)))

(defn- find-greatest [banks]
  (->> banks
       (map vector (range))
       reverse
       (apply max-key #(get % 1))))

(defn- step [banks index value]
  (let [n (count banks)]
    (->> (repeat 1)
         (take value)
         (concat (repeat (inc index) 0))
         (partition n n (repeat n 0))
         (apply map +)
         (mapv + banks))))

(defn part-1 [input]
  (loop [banks input
         seen #{}
         n 0]
    (if (contains? seen banks)
      n
      (let [seen (conj seen banks)
            [index value] (find-greatest banks)
            banks (assoc banks index 0)
            banks (step banks index value)]
        (recur banks seen (inc n))))))

(defn -main
  [& args]
  (println "part 1:" (part-1 input)))
