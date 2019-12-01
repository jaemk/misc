(ns aoc.d01
  (:require [aoc.registry :refer [register-day-fn!]]
            [aoc.utils :as u]))

(defn input [] (u/file->lines "../input/d01.txt"))

(defn mass->fuel [n]
  (-> (/ n 3)
      int
      (- 2)))

(defn part1
  ([] (part1 (input)))
  ([input-lines]
   (->> input-lines
        (map u/parse-int)
        (map mass->fuel)
        (reduce + 0))))


(defn module-mass->fuel [n]
  (loop [mass n
         total-fuel 0]
    (let [fuel-mass (mass->fuel mass)]
      (if (<= fuel-mass 0)
        total-fuel
        (recur fuel-mass (+ total-fuel fuel-mass))))))

(defn part2
  ([] (part2 (input)))
  ([input-lines]
   (->> input-lines
        (map u/parse-int)
        (map module-mass->fuel)
        (reduce + 0))))

(defn all []
  (println "** Day 1 **")
  (print   "  p1:")
  (let [{r :res t :ms} (u/ex-time (part1))]
    (println (format "%s, %sms" r t)))

  (print   "  p2:")
  (let [{r :res t :ms} (u/ex-time (part2))]
    (println (format "%s, %sms" r t)))
  )

(register-day-fn! all)
