(ns day4-test
  (:require [day4 :as sut]
            [clojure.test :as t]))

(t/deftest day4-part1
  (t/are [x y] (= x y)
    1 (sut/solve1 "111111-111111")
    0 (sut/solve1 "223450-223450")
    0 (sut/solve1 "123789-123789")))
