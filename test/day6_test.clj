(ns day6-test
  (:require [day6 :as sut]
            [clojure.test :as t]))

(t/deftest day6-part1
  (t/is (= 42 (sut/solve1 "resources/day6-test.txt"))))

(t/deftest day6-part2
  (t/is (= 4 (sut/solve2 "resources/day6-test2.txt"))))
