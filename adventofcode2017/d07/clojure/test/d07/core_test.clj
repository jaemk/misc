(ns d07.core-test
  (:require [clojure.test :refer :all]
            [d07.core :refer :all]))


(def test-input
"
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
")


(deftest a-test
  (testing "part1"
    (is (= 13 (count (parse test-input))))
    (is (= "tknk" (part-1 test-input)))))
