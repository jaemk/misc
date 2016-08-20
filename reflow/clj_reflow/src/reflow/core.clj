(ns reflow.core)

(defn read-file [filename]
  (as-> (slurp (str "resources/" filename)) _
      (clojure.string/split _ #"\n")
      (map clojure.string/trim _)))

(defn lines->words [lines]
  (mapcat #(clojure.string/split % #" ") lines))

(defn sized-words
  "map words into vectors containing
  [word-length word]"
  [words]
  (map (fn [word] [(count word) word]) words))

(defn limiter
  "maxlen to limit to
  acc is a list of completed lines, first is the current
  each line is a vector containin [size-of-line-int list-of-words]"
  [maxlen acc [size word]]
  (let [line (first acc)       ; current line
        line-size (first line) ; size of current line
        content (last line)    ; content of current line
        new-line-size (+ line-size size)]
    (cond
      (zero? size) (concat [[0 '()] [size '()]] acc)
      (-> new-line-size
          (+ (count content))
          (<= maxlen)) (cons [new-line-size (conj content word)]
                         (rest acc))
      :else (cons [size (list word)] acc))))

(defn limit
  "Reflow text content to keep lines within maxlen."
  [content maxlen]
  (->> (lines->words content)
       (sized-words)
       (reduce (partial limiter maxlen) (list [0 '()]))
       (reduce (fn [acc line]
                   (conj acc (into '() (last line))))
               '())
       (map #(clojure.string/join " " %))
       (clojure.string/join "\n")))

(defn space-indices
  "Generate an infinite seq of space
  indices cycling from the outside in."
  [num-words]
  (->> (filter odd? (range num-words))
       (#(split-at (/ (count %) 2) %))
       ((fn [[top bot]]
            (cond
              (empty? top) top
              (empty? bot) top
              :else (interleave
                      (cycle top)
                      (cycle (reverse bot))))))))

(defn pad-line
  "Increase space sizes from the outside in
  to padd the line to the maxlen size."
  [maxlen line]
  (let [line (->> (clojure.string/split line #" ")
                  (interpose " ")
                  (into []))
        len (apply + (map count line))
        num-words (count line)
        spaces (space-indices num-words)]
    (loop [maxlen maxlen
           line line
           len len
           spaces spaces]
     (let [space (first spaces)
           newspaces (rest spaces)]
       (if (and (< len maxlen) (not (nil? space)))
         (recur maxlen (update line space #(str % " ")) (inc len) newspaces)
         line)))))

(defn justify
  "Reflow text to maxlen limit and
  pad with spaces from the outside
  in to fit to the maxlen size."
  [content maxlen]
  (as-> (limit content maxlen) _
      (clojure.string/split _ #"\n")
      (map #(pad-line maxlen %) _)
      (map clojure.string/join _)
      (clojure.string/join "\n" _)))

(defn -main [& args]
  (let [content (read-file "input.txt")]
    (println "Text-reflowing...")
    (println (limit content 40))
    (println "\n\nJustifying...")
    (println (justify content 40))))

