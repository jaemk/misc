(ns d01.core
  (:require [clojure.string :as string]))

(def input-file "../input.txt")

(defn parse-input [s]
  (as-> s v
      (string/trim-newline v)
      (string/split v #"")
      (mapv #(Integer/parseInt %) v)))

(def input
  (as-> input-file v
        (slurp v)
        (parse-input v)))

(defn de-captcha [acc [a b]]
  (+ acc (if (= a b) a 0)))


(defn part-1 [input]
  (let [hd (first input)
        tail (rest input)
        rotated (concat tail [hd])
        zipped (map vector input rotated)]
    (reduce de-captcha
            0
            zipped)))

(defn part-2 [input]
  (let [size (count input)
        step (quot size 2)
        front (take step input)
        back (drop step input)
        shifted (concat back front)
        zipped (map vector input shifted)]
    (reduce de-captcha 0 zipped)))

(defn main [& args]
  (let [p1 (part-1 input)]
    (println "Part 1:" p1))
  (let [p2 (part-2 input)]
    (println "Part 2:" p2)))

(main)
