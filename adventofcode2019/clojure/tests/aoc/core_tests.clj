(ns aoc.core-tests
  (:require [aoc.registry :as reg]
            [aoc.utils :as u]
            [aoc.core])
  (:use [clojure.test]))

(defn require-all [namespaces]
  (apply require namespaces)
  namespaces)

(defn day-ns->test-ns [ns']
  (-> (str ns' "-tests")
      symbol))

(defn test-all []
  (->> @reg/days
       vals
       (map day-ns->test-ns)
       require-all
       (apply run-tests)))

(defn -main []
  (println "Loading tests")
  (test-all))
