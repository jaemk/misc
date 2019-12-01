(ns aoc.dev
  (:require [aoc.core :as aoc]
            [aoc.registry :as reg])
  (:use [clojure.repl]
        [clojure.tools.namespace.repl :only [refresh]]
        [clojure.test]))

(def test-namespaces
  ['aoc.d01-tests
   ])

(defn publics [ns']
  (keys (ns-publics ns')))

(defn rr []
  (refresh))

(defn tt []
  (rr)
  (apply run-tests test-namespaces))

(defn main []
  (aoc/-main))
