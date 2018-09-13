(ns d06.core-test
  (:require [clojure.test :refer :all]
            [d06.core :refer :all]))

(deftest solves
  (testing "solver"
    (let [res (solve [0 2 7 0])]
      (is (= (:count res) 5))
      (is (= (:diff res) 4)))))
