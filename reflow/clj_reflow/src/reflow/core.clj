(ns reflow.core)

(defn read-file [filename]
  (as-> (slurp (str "resources/" filename)) _
      (clojure.string/split _ #"\n")
      (map clojure.string/trim _)))

(defn lines->words [lines]
  (mapcat #(clojure.string/split % #" ") lines))

(defn sized-words [words]
  (map (fn [word] [(count word) word]) words))

(defn limiter [maxlen acc [size word]]
  (let [line (first acc)
        line-size (first line)
        content (last line)
        new-line-size (+ line-size size)]
    (cond
      (zero? size) (concat [[0 '()] [size '()]] acc)
      (-> new-line-size
          (+ (count content))
          (<= maxlen)) (cons [new-line-size (conj content word)]
                         (rest acc))
      :else (cons [size (list word)] acc))))

(defn limit [content maxlen]
  (->> (lines->words content)
       (sized-words)
       (reduce (partial limiter maxlen) (list [0 '()]))
       (reduce (fn [acc line]
                   (conj acc (into '() (last line))))
               '())
       (map #(clojure.string/join " " %))
       (clojure.string/join "\n")))

(defn padder [maxlen line len spaces])

(defn pad-line
  [maxlen line]
  (let [line (->> (clojure.string/split line #" ")
                  (interpose " ")
                  (into []))
        len (apply + (map count line))
        num-words (count line)
        spaces (->> (filter odd? (range num-words))
                    (split-at (/ num-words 2))
                    ((fn [inds] (interleave (first inds) (reverse (last inds))))))]
    ;(reduce (partial padder maxlen) )
    (loop [maxlen maxlen
           line line
           len len
           spaces spaces]
     (let [space (first spaces)
           newspaces (rest spaces)]
       (println (str "len: " len ", spaces: " spaces))
       (if (or (>= len maxlen) (empty? spaces))
         line
         (recur maxlen (update line space #(str % " ")) (inc len) newspaces))))))

(defn justify [content maxlen]
  (as-> (limit content maxlen) _
      (clojure.string/split _ #"\n")
      (map #(pad-line maxlen %) _)))

(defn -main [& args]
  (let [content (read-file "input.txt")]
    (println "Text-reflowing...")
    (println (limit content 40))
    (println "Justifying...")
    (println (justify content 40))))

