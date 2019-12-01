(ns aoc.d02
  (:require [aoc.utils :as u]
            [byte-streams :as bs]
            [clojure.math.combinatorics :as combo]))

(defn load-input []
  (-> (slurp "../input/d02.txt")
      clojure.string/split-lines))


(defn count-occur [res line]
  (let [fr (frequencies line)
        counts (vals fr)
        add-2 (if (some #{2} counts) 1 0)
        add-3 (if (some #{3} counts) 1 0)]
    (-> res
        (update :2 #(+ % add-2))
        (update :3 #(+ % add-3)))))

(defn checksum [lines]
  (let [{two :2
         three :3} (reduce count-occur {:2 0 :3 0} lines)]
    (* two three)))

(defn part1
  ([] (part1 (load-input)))
  ([input-lines]
   (checksum input-lines)))


(defn one-away? [a-str b-str]
  (let [a-bytes (bs/to-byte-array a-str)
        b-bytes (bs/to-byte-array b-str)]
    (as-> (map bit-xor a-bytes b-bytes) _
      (into #{} _)
      (and (= (count _) 2)
           (some #{0} _)))))

(defn remove-diff [a-str b-str]
  (->> (map vector a-str b-str)
       (filter #(apply = %))
       (map (fn [[a-char b-char]] a-char))
       clojure.string/join))

(defn find-similar [lines]
  (loop [perms (combo/combinations lines 2)]
    (let [[[a b] & more] perms]
      (cond
        (nil? a) nil
        (one-away? a b) (remove-diff a b)
        :else (recur more)))))

(defn part2
  ([] (part2 (load-input)))
  ([input-lines]
   (find-similar input-lines)))

(defn all []
  (println "** Day 2 **")
  (print   "  p1:")
  (let [{r :res t :ms} (u/ex-time (part1))]
    (println (format "%s, %sms" r t)))
  (print   "  p2:")
  (let [{r :res t :ms} (u/ex-time (part2))]
    (println (format "%s, %sms" r t)))
  )
