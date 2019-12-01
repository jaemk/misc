(ns aoc.core
  (:require [aoc.d01 :as d01]
            [aoc.d02 :as d02]))

(defn run []
  (d01/all)
  (d02/all))

(defn -main []
  (run))
