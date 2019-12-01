(ns aoc.dev
  (:require [aoc.core :as aoc]
            [aoc.utils :as u]
            [aoc.registry :as reg]
            [aoc.core-tests :as t])
  (:use [clojure.repl]
        [clojure.tools.namespace.repl :only [refresh]]))

(defn publics [ns']
  (keys (ns-publics ns')))

(defn tt []
  (t/test-all))

(defn main []
  (aoc/-main))
