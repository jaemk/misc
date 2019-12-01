(ns aoc.registry)

"A map of day numbers to day namespaces"
(defonce days
  (atom (sorted-map)))

(defn register-day! [day-num day-ns]
  (swap! days #(assoc % day-num day-ns)))

