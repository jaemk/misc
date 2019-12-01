(ns aoc.registry)

(def day-fns (atom []))

(defn register-day-fn! [func]
  (println "registering " func)
  (swap! day-fns #(conj % func)))

