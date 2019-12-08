(ns day5-test
  (:require [day5 :as sut]
            [day2]
            [clojure.test :as t]))


(t/deftest day5-part1
  (t/is (= "77\n" (with-in-str "77"
                    (with-out-str
                      (sut/execute [3 0 4 0 99] 0))))))

(t/deftest day5-part2 
  (t/is (= "1\n" 
           (with-out-str
             (-> "3,9,8,9,10,9,4,9,99,-1,8" day2/parse-intcodes
                 (sut/solve2 "8")))))
  (t/is (= "0\n" 
           (with-out-str
             (-> "3,9,8,9,10,9,4,9,99,-1,8" day2/parse-intcodes
                 (sut/solve2 "9")))))
  (t/is (= "1\n" 
           (with-out-str
             (-> "3,9,7,9,10,9,4,9,99,-1,8" day2/parse-intcodes
                 (sut/solve2 "7")))))
  (t/is (= "0\n" 
           (with-out-str
             (-> "3,9,7,9,10,9,4,9,99,-1,8" day2/parse-intcodes
                 (sut/solve2 "9")))))
  (t/is (= "1\n" 
           (with-out-str
             (-> "3,3,1108,-1,8,3,4,3,99" day2/parse-intcodes
                 (sut/solve2 "8")))))
  (t/is (= "0\n" 
           (with-out-str
             (-> "3,3,1108,-1,8,3,4,3,99" day2/parse-intcodes
                 (sut/solve2 "0")))))
  (t/is (= "1\n" 
           (with-out-str
             (-> "3,3,1107,-1,8,3,4,3,99" day2/parse-intcodes
                 (sut/solve2 "0")))))
  (t/is (= "0\n" 
           (with-out-str
             (-> "3,3,1107,-1,8,3,4,3,99" day2/parse-intcodes
                 (sut/solve2 "9"))))))

(t/deftest day5-part2-jump
  (t/is (= "0\n" 
           (with-out-str
             (-> "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" day2/parse-intcodes
                 (sut/solve2 "0")))))
  (t/is (= "1\n" 
           (with-out-str
             (-> "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" day2/parse-intcodes
                 (sut/solve2 "1"))))))

(t/deftest day5-part2-larger-example
  (t/is (= "999\n" 
           (with-out-str
             (-> "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" day2/parse-intcodes
                 (sut/solve2 "1")))))
  (t/is (= "1000\n" 
           (with-out-str
             (-> "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" day2/parse-intcodes
                 (sut/solve2 "8")))))
  (t/is (= "1001\n" 
           (with-out-str
             (-> "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" day2/parse-intcodes
                 (sut/solve2 "900"))))))
