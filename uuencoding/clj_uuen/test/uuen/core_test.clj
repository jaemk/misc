(ns uuen.core-test
  (:require [clojure.test :refer :all]
            [uuen.core :refer :all]))

(defn backfill [string]
  (clojure.string/join
    (reduce #(conj %1 %2)
            (seq string)
            (repeat (- 8 (count (seq string))) "0"))))

(deftest int-to-bin
  (testing "integer to binary string"
    (is (= (map #(int->bin % 8) (range 256))
           (map backfill
             (map #(Integer/toBinaryString %) (range 256)))))))

(deftest int-to-bin-8bit
  (testing "binary strings have 8 chars"
    (is (apply = 8 (map count (map #(int->bin % 8) (range 256)))))))

(deftest int-to-bin-6bit
  (testing "binary strings have 6 chars"
    (is (apply = 6 (map count (map #(int->bin % 6) (range 64)))))))

