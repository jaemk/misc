(ns trolls.core-test
  (:require [clojure.test :refer :all]
            [trolls.core :refer :all]))

(def maze (read-in-maze "resources/basic_maze.txt"))

(deftest loads
  (testing "basicload"
    (is (not (empty? maze)))))

(deftest iswall
  (testing "wallcheck"
    (is (= true (wall? 0 0 maze)))))

(deftest pushing
  (testing "wallpushing"
    (is (can-push? 2 1 :right maze))))

