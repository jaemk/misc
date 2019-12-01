(ns aoc.d02
  (:require [aoc.utils :as u]))

(defn input [] (u/file->lines "../input/d02.txt"))

(defn part1
  ([] (part1 (input)))
  ([input-lines] nil))

(defn part2
  ([] (part2 (input)))
  ([input-lines] nil))


(defn all []
  (println "** Day 2 **")
  (print   "  p1:")
  (let [{r :res t :ms} (u/ex-time (part1))]
    (println (format "%s, %sms" r t)))

  (print   "  p2:")
  (let [{r :res t :ms} (u/ex-time (part2))]
    (println (format "%s, %sms" r t)))
  )

