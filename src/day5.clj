(ns day5
  (:require [day2]
            [day4]
            [clojure.string :as str]))

(defn execute-input [[ints ip]]
  (->> (Integer/parseInt (read-line))
       (aset ints (aget ints (inc ip))))
  [ints (+ 2 ip)])

(defn execute-output [[ints ip]]
  (println (aget ints (aget ints (inc ip))))
  [ints (+ 2 ip)])

(defn parse-instruction [intcode]
  (let [digits (day4/int->digits intcode)
        [a b c d e] (concat (repeat (- 5 (count digits)) 0) digits)]
    {:A a :B b :C c :opcode (+ (* d 10) e)}))

(defn execute-op [[ints ip] op-fn instruction]
  (let [b (:B instruction) c (:C instruction)]
    (if (= 0 b c)
      (day2/execute-op [ints ip] op-fn)
      (let [arg1 (if (= 0 c) (aget ints (aget ints (+ 1 ip)))
                     (aget ints (+ 1 ip)))
            arg2 (if (= 0 b) (aget ints (aget ints (+ 2 ip)))
                     (aget ints (+ 2 ip)))]
        (aset ints (aget ints (+ ip 3)) (op-fn arg1 arg2))
        [ints (+ 4 ip)]))))

(defn execute
  ([intcodes ip] (execute [(int-array intcodes) 0]))
  ([[ints ip]]
   (let [instruction (parse-instruction (aget ints ip))]
     (case (:opcode instruction) 
       1 (recur (execute-op [ints ip] + instruction))
       2 (recur (execute-op [ints ip] * instruction))
       3 (recur (execute-input [ints ip]))
       4 (recur (execute-output [ints ip]))
       (day2/execute [ints ip])))))

(defn solve1 [intcodes]
  (-> (with-out-str
        (with-in-str "1"
          (execute intcodes 0)))
      (str/split-lines)
      last))

(defn -main [& args]
  (let [intcodes (-> (slurp "resources/day5-input.txt")
                     day2/parse-intcodes)]
    (println "part 1:" (solve1 intcodes))))

(comment
  (-> "resources/day5-input.txt" slurp day2/parse-intcodes solve1)
  ;; => "16489636"
  )
