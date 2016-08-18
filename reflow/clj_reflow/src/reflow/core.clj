(ns reflow.core)

(defn read-file [filename]
  (as-> (slurp (str "resources/" filename)) _
      (clojure.string/split _ #"\n")
      (map clojure.string/trim _)))

(defn -main [& args]
 (println (read-file)))
