(defproject d03 "0.1.0-SNAPSHOT"
  :description "advent of code"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot d03.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
