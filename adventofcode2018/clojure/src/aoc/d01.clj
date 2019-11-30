(ns aoc.d01
  (:require [aoc.utils :as u]))


(def load-input
  (memoize
    (fn []
      (-> (slurp "../input/d01.txt")
          clojure.string/split-lines))))


(defn get-op [op-char]
  ({\+ +
   \- -} op-char))


(defn parse [line]
  (let [[op-char] line
        op (get-op op-char)
        n-str (subs line 1)
        n (u/parse-int n-str)]
    (op n)))


(defn reduce-lines [lines]
  (reduce + 0 (map parse lines)))

(defn part1
  ([] (part1 (load-input)))
  ([input-lines]
   (reduce-lines input-lines)))


(defn -find-cycle
  [seen value [hd & tl]]
  (let [value (+ value hd)]
    (if (seen value)
      value
      (recur (conj seen value) value tl))))

(defn find-cycle
  [lines]
  (let [iter (cycle (map parse lines))]
    (-find-cycle #{} 0 iter)))

(defn part2
  ([] (part2 (load-input)))
  ([input-lines]
   (find-cycle input-lines)))


(defn all []
  (println "** Day 1 **")
  (print   "  p1:")
  (let [{r :res t :ms} (u/ex-time (part1))]
    (println (format "%s, %sms" r t)))

  (print   "  p2:")
  (let [{r :res t :ms} (u/ex-time (part2))]
    (println (format "%s, %sms" r t))))
