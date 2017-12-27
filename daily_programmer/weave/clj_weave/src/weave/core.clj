(ns weave.core)

(defn weave [weaved coll]
  (->> (cycle weaved)
       (map vector coll)
       flatten
       drop-last))

(defn bracket [group brackets]
  (let [size (max (count group) (/ (count brackets) 2))]
    (->> (partition 2 2 "|" brackets)
         (cycle)
         (map vector (cycle group))
         (take size)
         (map (fn [[w coll]]
                   (-> (weave [w] coll)
                       clojure.string/join))))))


;; -- Evaluate from text file --
(defn display [result]
  (println "Output:")
  (println (clojure.string/join "\n" result)))

(defn process [blob]
  (println "\nProcessing:")
  (println blob)
  (let [parts (clojure.string/split blob #"\n")
        fn-name (clojure.string/lower-case (first parts))
        ops (rest parts)]
    (case fn-name
      "bracket" (display (bracket (drop-last ops) (last ops)))
      "weave" (display (weave (first ops) (rest ops)))
      (str "invalid function name" fn-name))))

(defn run-from-file! [source]
  (-> (slurp source)
      (clojure.string/trim)
      (clojure.string/split #"\n\n")
      (#(mapv process %))))

(defn -main [& args]
  (println "You could also run the tests..")
  (println "Reading input from resources/input.txt ...")
  (run-from-file! "resources/input.txt"))
