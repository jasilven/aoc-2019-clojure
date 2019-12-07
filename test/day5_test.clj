(ns day5-test
  (:require [day5 :as sut]
            [clojure.test :as t]))


(t/deftest day5-part1
  (t/is (= "77\n" (with-in-str "77"
                    (with-out-str
                      (sut/execute [3 0 4 0 99] 0))))))
