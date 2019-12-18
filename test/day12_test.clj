(ns day12-test
  (:require [day12 :as sut]
            [clojure.test :as t]))

(t/deftest day12-part1
  (let [names [{:name "Io"} {:name "Europa"} {:name "Ganymede"} {:name "Callisto"}]
        moons1 (sut/parse-moons "resources/day12-test.txt" names)
        moons2 (sut/parse-moons "resources/day12-test2.txt" names)]
    (t/are [x y] (= x y)
      179 (sut/total-energy moons1 10)
      1940 (sut/total-energy moons2 100))))
