(ns aoc.d02
  (:require [aoc.utils :as u]
            [aoc.registry :refer [register-day!]]
            [clojure.math.combinatorics :as combo]))

(register-day! 2 *ns*)

(defn input [] (u/f->s "../input/d02.txt"))
(def load-mem
  (memoize
    (fn [in]
      (->> (clojure.string/split in #",")
        (map u/parse-int)
        vec))))


(defn prepare [input-vec noun verb]
  (-> input-vec
      (assoc 1 noun)
      (assoc 2 verb)))

(defn get-op [op-code]
  (if-let [op ({1 + 2 *} op-code)]
    op
    (throw
      (ex-info
        (format "invalid op-code %s" op-code)
        {:op-code op-code
         :key :invalid-op-code}))))

(defn run-to-complete
  ([input-vec] (run-to-complete input-vec 0))
  ([v pos]
   (if (= (nth v pos) 99)
     v
     (let [op (-> (nth v pos) get-op)
          ind-a (nth v (+ 1 pos))
          input-a (nth v ind-a)

          ind-b (nth v (+ 2 pos))
          input-b (nth v ind-b)

          ind-res (nth v (+ 3 pos))
          next-pos (+ 4 pos)
          v (->> (op input-a input-b)
                 (assoc v ind-res))]
       (recur v next-pos)))))

(defn part1
  ([] (part1 (input)))
  ([input]
   (-> (load-mem input)
       (prepare 12 2)
       run-to-complete
       first)))


;; n v
;; 0 0 -> 1114711
;; 1 0 -> +216000
;; 0 1 -> +000001
;;
;; want 19690720
;; -     1114711
;; diff 18576009
;;
;; 18576009 / 216000 -> 86
;; 18576009 - (216000 * 86) -> 9
(defn part2 []
  8609)


(defn all []
  (println "** Day 2 **")
  (print   "  p1:")
  (let [{r :res t :ms} (u/ex-time (part1))]
    (println (format "%s, %sms" r t)))

  (print   "  p2:")
  (let [{r :res t :ms} (u/ex-time (part2))]
    (println (format "%s, %sms" r t)))
  )

