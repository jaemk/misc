(ns d04.core
  (:gen-class))

(def input (slurp "../input.txt"))

(defn unique-parts? [s]
  (let [parts (clojure.string/split s #" ")
        n-parts (count parts)
        n-unique (count (set parts))]
    (= n-parts n-unique)))

(defn part-1 [pws]
  (->> (clojure.string/trim input)
       clojure.string/split-lines
       (filter #(unique-parts? %))
       count))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
