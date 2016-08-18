(ns uuen.core)

;; --- string to binary(string) ----
(defn int->bin
  "Convert integer into a binary n-bit string
  specified by keyword :bits, defaults to 8."
  [integer bits]
  (loop [n integer
         bin nil]
    (if (> 1 n)
      (clojure.string/join
        (reduce #(conj %1 %2) bin (repeat (- bits (count bin)) 0)))
      (recur (int (/ n 2))
             (if (even? n)
               (conj bin 0)
               (conj bin 1))))))

(defn str->ints
  "Convert string or seq of chars into seq of ord-ints"
  [string]
  (reduce #(conj %1 (int %2)) [] (seq string)))

(defn str->bin
  "Convert string or seq of chars into binary string"
  [string & {:keys [bits] :or {bits 8}}]
  (->> (str->ints string)
       (map #(int->bin % bits))
       clojure.string/join))

;; --- binary(string) to integer ---
(defn char->int [c]
  (Integer. (str c)))

(defn bit->int
  "Convert an indexed bit of a byte
  into it's base10 integer value."
  [ind bit]
  (cond
    (zero? bit) 0
    (zero? ind) 1
    :else (apply * (repeat ind 2))))

(defn bin->int
  "Convert binary string into base10 integer."
  ([bin-str]
   (->> (reverse bin-str)
        (map vector (range))
        (reduce bin->int 0)))
  ([acc [ind bit-char]]
   (->> (char->int bit-char)
        (bit->int ind)
        (+ acc))))

;; --- uuencoder ---
(defn encode-chunk [bin-str]
  (->> (partition 6 6 nil bin-str)
       (map bin->int)
       (map #(+ 32 %))
       (map char)
       (clojure.string/join)))

(defn encode [content]
  "Encode actual content. Groups content by 3
  byte chunks, padding with zero (not char \0)"
  (->> (partition 3 3 (repeat 0) content)
       (map str->bin)
       (map encode-chunk)
       (clojure.string/join)))

(defn format-lines
  "Add the extra formatting stuff around
  the encoded content."
  [filename encoded]
  (let [lines [(str "begin 644 " filename)]]
    (->> (partition 60 60 nil encoded)
         (map #(conj % (->> (count %)  ;; prepend with the encoded-line's <length>
                            (* 3/4)
                            (+ 32)
                            (int)
                            (char))))
         (map clojure.string/join)
         (concat lines)
         (#(concat % ["`\nend"]))
         (clojure.string/join "\n"))))

(defn uuencode [& {:keys [filename content]
                   :or {filename nil content nil}}]
  (->> (cond  ; prefer explicit content
         content (encode content)
         filename (-> (str "resources/" filename)
                      (slurp)
                      (encode)))
       (format-lines filename)))


;; --- decoder ---
(defn decode-line [encoded-line]
  (->> (partition 4 4 nil encoded-line)
       (map str->ints)
       (map (fn [col] (map #(- % 32) col)))
       (map #(str->bin % :bits 6))
       (map (fn [chunked]
              (->> (partition 8 8 nil chunked)
                   (map bin->int)
                   (map char)
                   clojure.string/join)))
       clojure.string/join))

(defn decode [encoded-content]
  (->> (clojure.string/split encoded-content #"\n")
       (rest)
       (drop-last 2)
       (map rest)
       (map decode-line)
       clojure.string/join))


;; --- main ---
(defn -main [& args]
  (->> (str "I feel very strongly about you doing duty. Would you give me a little more "
            "documentation about your reading in French? I am glad you are happy - but I "
            "never believe much in happiness. I never believe in misery either. Those are "
            "things you see on the stage or the screen or the printed pages, they never really happen to you in life.")
       (uuencode :filename "file.txt" :content)
       ((fn [enc] (println enc) enc))
       (decode)))

