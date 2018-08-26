(ns d02.core
  (:gen-class))

(def input-file "../input.txt")

(def input-str
  (as-> input-file v
    (slurp v)
    (clojure.string/trim-newline v)))

(defn parse-input [s]
  (as-> s v
    (clojure.string/split-lines v)
    (map
      (fn [row] (map #(Integer/parseInt %) (clojure.string/split row #"\s")))
      v)))


(defn new-min-max [n] {:min n :max n})

(defn min-max [acc n]
  (let [acc (if (some nil? (vals acc)) (new-min-max n) acc)
        min' (:min acc)
        max' (:max acc)]
    (cond
      (< n min') (assoc acc :min n)
      (> n max') (assoc acc :max n)
      :else acc)))

(defn min-max-diff [row]
  (as-> row v
    (reduce min-max (new-min-max nil) v)
    (- (:max v) (:min v))))

(defn part-1 [input-rows]
  (reduce #(+ %1 (min-max-diff %2)) 0 input-rows))


(defn divisible-quot [row]
  (->> (for [a row
             b row
             :when (not (= a b))
             :when (= 0 (rem a b))]
         [a b])
    (first)
    (apply quot)))

(defn part-2 [input-rows]
  (reduce #(+ %1 (divisible-quot %2)) 0 input-rows))

(defn -main
  [& args]
  (println "** d02 **")
  (let [p1 (part-1 (parse-input input-str))]
    (println "Part 1:" p1))
  (let [p2 (part-2 (parse-input input-str))]
    (println "Part 2:" p2)))
