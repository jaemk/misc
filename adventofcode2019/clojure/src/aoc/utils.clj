(ns aoc.utils)

(defn dbg [n]
  (println n)
  n)

(defn call [func]
  (func))

(def file->lines
  (memoize
    (fn [file]
      (-> (slurp file)
          clojure.string/split-lines))))

(defn parse-int [s]
  (Integer. s))

(defmacro ex-time [& body]
  `(let [start# (System/currentTimeMillis)
         r# ~@body
         end# (System/currentTimeMillis)]
     {:res r#
      :ms (- end# start#)}))
