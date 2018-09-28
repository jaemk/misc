(ns d07.core
  (:require [clojure.string :as string]
            [clojure.set])
  (:gen-class))

(def input-raw (slurp "../input.txt"))

(defn make-node
  [name- weight children]
  {:name name-
   :weight weight
   :children children})

(defn- parse-children
  "Parse the trailing line info into children names.
  `maybe-kids` will either be `nil` or '-> name1, name2, name3'"
  [maybe-kids]
  (let [kids (rest maybe-kids)]
    (mapv #(re-find #"\w+" %) kids)))

(defn parse-line-to-node
  "Parse an input line to a node
  `line` will be formatted as:
  'name (weight) -> child1, child2, child3'"
  [line]
  (let [parts (string/split line #" ")
        [name- weight & children] parts
        weight (-> (re-find #"\d+" weight) Integer.)
        children (parse-children children)]
    (make-node name- weight children)))

(defn parse
  "Parse input into a map of nodes"
  [s]
  (->> s
       string/trim
       string/split-lines
       (map parse-line-to-node)
       (reduce #(assoc %1 (:name %2) %2) {})))

(defn find-root
  "Find the root node of the tree"
  [nodes]
  (let [roots (->> nodes keys (apply hash-set))
        kids (->> nodes vals (mapcat :children) (apply hash-set))]
    (-> (clojure.set/difference roots kids)
        first)))

(defn part-1 [input-str]
  (-> (parse input-str)
      find-root))

(defn -main
  [& args]
  (println "Part 1:" (part-1 input-raw)))
