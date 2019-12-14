(ns day10-test
  (:require [day10 :as sut]
            [clojure.test :as t]))

(t/deftest day10-part1
  (t/are [x y] (= x y)
    8 (sut/solve1 "resources/day10-test1.txt")
    33 (sut/solve1 "resources/day10-test2.txt")
    35 (sut/solve1 "resources/day10-test3.txt")
    41 (sut/solve1 "resources/day10-test4.txt")
    210 (sut/solve1 "resources/day10-test5.txt")))
