(ns aoc.core
  (:require [aoc.registry :refer [day-fns]]
            [aoc.d01 :as d01]
            ))

(defn run []
  ((apply juxt @day-fns)))


(defn -main []
  (run))
