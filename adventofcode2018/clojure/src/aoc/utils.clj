(ns aoc.utils)

(defn parse-int [s]
  (Integer. s))

(defmacro ex-time [& body]
  `(let [start# (System/currentTimeMillis)
         r# ~@body
         end# (System/currentTimeMillis)]
     {:res r#
      :ms (- end# start#)}))
