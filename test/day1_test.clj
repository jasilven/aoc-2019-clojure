(ns day1-test
  (:require [day1 :as sut]
            [clojure.test :as t]))

(t/deftest day1-part1
  (t/are [x y] (= x y)
    33583 (sut/fuel-required 100756)
    654 (sut/fuel-required 1969)
    2 (sut/fuel-required 12)
    2 (sut/fuel-required 12)
    2 (sut/fuel-required 14)))

(t/deftest day1-part2
  (t/are [x y] (= x y)
    2 (sut/fuel-required2 14)
    966 (sut/fuel-required2 1969)
    50346 (sut/fuel-required2 100756)))
