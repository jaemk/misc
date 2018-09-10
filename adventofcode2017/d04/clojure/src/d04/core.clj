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


(defn no-anagrams? [s]
  (let [parts (clojure.string/split s #" ")
        n-parts (count parts)
        freqs (map frequencies parts)
        n-unique (count (set freqs))]
    (= n-parts n-unique)))

(defn part-2 [pws]
  (->> (clojure.string/trim input)
       clojure.string/split-lines
       (filter #(no-anagrams? %))
       count))

(defn -main
  [& args]
  (println "part 1:" (part-1 input))
  (println "part 2:" (part-2 input)))

; (-main)
