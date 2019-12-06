(ns day3-test
  (:require [day3 :as sut]
            [clojure.test :as t]))

(t/deftest day3-part1
  (t/are [x y] (= x y)
    6 (sut/solve1 "R8,U5,L5,D3" "U7,R6,D4,L4")
    135 (sut/solve1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    159 (sut/solve1 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")))

(t/deftest day3-part2
  (t/are [x y] (= x y)
    30 (sut/solve2 "R8,U5,L5,D3" "U7,R6,D4,L4")
    410 (sut/solve2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    610 (sut/solve2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")))
