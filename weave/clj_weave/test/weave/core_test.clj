(ns weave.core-test
  (:require [clojure.test :refer :all]
            [weave.core :refer :all]))

(deftest weave1
  (testing "weave 1"
    (is (=
         (weave [11] [0 1 2 3])
         [0 11 1 11 2 11 3]))))

(deftest weave2
  (testing "weave 2"
    (is (=
         (weave [11 12] [0 1 2 3])
         [0 11 1 12 2 11 3]))))

(deftest weave3
  (testing "weave 3"
    (is (=
         (weave [11 12 13] [0 1])
         [0 11 1]))))

(deftest bracket1
  (testing "bracket 1"
    (is (=
         (bracket "abc" "()")
         ["(a)" "(b)" "(c)"]))))

(deftest bracket2
  (testing "bracket 2"
    (is (=
         (bracket "+-" [2 3 4 5 6 7])
         ["2+3" "4-5" "6+7"]))))

(deftest bracket3
  (testing "bracket 3"
    (is (=
         (bracket ["2+3" "4-5" "6+7"] "()")
         ["(2+3)" "(4-5)" "(6+7)"]))))

(deftest weave4
  (testing "weave 4"
    (is (=
         (weave "*" ["(2+3)" "(4-5)" "(6+7)"])
         ["(2+3)" \* "(4-5)" \* "(6+7)"]))))

