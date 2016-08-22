(ns fingercount.core-test
  (:require [clojure.test :refer :all]
            [fingercount.core :refer :all]))

(def cases {"0111011100" 37
            "1010010000" :invalid
            "0011101110" 73
            "0000110000" 55
            "1111110001" :invalid})

(deftest finger-tests
  (testing "Fingers"
    (is (every? true?
          (map #(= (finger (first %)) (last %)) cases)))))

