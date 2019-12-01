set -e

day="$1"

if [ -z "$day" ]; then
    echo "please specify day number"
    exit 1
fi

day_s=d`printf "%02d" $day`
src_file=src/aoc/$day_s.clj
test_file=tests/aoc/"$day_s"_tests.clj

echo "creating source file: $src_file"
_src=\
"(ns aoc.$day_s
  (:require [aoc.utils :as u]
            [aoc.registry :refer [register-day!]]))

(register-day! $day *ns*)

(defn input [] (u/file->lines \"../input/$day_s.txt\"))

(defn part1
  ([] (part1 (input)))
  ([input-lines] nil))

(defn part2
  ([] (part2 (input)))
  ([input-lines] nil))


(defn all []
  (println \"** Day $day **\")
  (print   \"  p1:\")
  (let [{r :res t :ms} (u/ex-time (part1))]
    (println (format \"%s, %sms\" r t)))

  (print   \"  p2:\")
  (let [{r :res t :ms} (u/ex-time (part2))]
    (println (format \"%s, %sms\" r t)))
  )
"
echo "$_src" > $src_file

echo "creating test file: $test_file"
_test=\
"(ns aoc.$day_s-tests
  (:require [aoc.$day_s]
            [clojure.test :refer :all]))

(deftest p1-1
  (is (= nil (aoc.$day_s/part1 [\"in\"])))
  )

(deftest p1-2
  (is (= nil (aoc.$day_s/part2 [\"in\"])))
  )
"
echo "$_test" > $test_file

echo "retrieving input for day $day"
../get-input.sh $day ../input

