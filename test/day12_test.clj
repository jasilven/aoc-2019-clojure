(ns day12-test
  (:require [day12 :as sut]
            [clojure.test :as t]))

(t/deftest day12-part1
  (let [moons1 (sut/parse-moons "resources/day12-test.txt")
        moons2 (sut/parse-moons "resources/day12-test2.txt")]
    (t/are [x y] (= x y)
      179 (sut/total-energy moons1 10)
      1940 (sut/total-energy moons2 100))))

(t/deftest day12-part2
  (t/are [x y] (= x y)
    2772 (sut/solve2 "resources/day12-test.txt")))
