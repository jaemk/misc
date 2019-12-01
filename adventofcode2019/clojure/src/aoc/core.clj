(ns aoc.core
  (:require [aoc.registry :refer [days]]
            [aoc.utils :as u]
            [aoc.d01]
            ))

(defn day-ns->all-fn [ns']
  (if-let [all-fn (ns-resolve ns' 'all)]
    all-fn
    (throw
      (ex-info
        (format "namespace: %s is missing an `all` method" ns')
        {:namespace ns'
         :key :missing-all-fn}))))

(defn run-all []
  (->> @days
       vals
       (map day-ns->all-fn)
       (apply juxt)
       u/call))


(defn -main []
  (run-all))
