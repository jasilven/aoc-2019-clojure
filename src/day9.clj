(ns day9
  (:require [day4]
            [clojure.core.async :as async :as a]
            [clojure.string :as str]))

(defn parse-program [s]
  (let [intcodes (-> s str/trim-newline (str/split #","))]
    (->> (map #(Long/parseLong %) intcodes)
         (interleave (range))
         (partition 2)
         (map #(into [] %))
         (into {}))))

(defn parse-intcode [intcode]
  (let [digits (day4/int->digits intcode)
        [a b c d e] (concat (repeat (- 5 (count digits)) 0)
                            digits)]
    {:a a :b b :c c :de (+ (* d 10) e)}))

(defn mem-ptr [context mode i]
  (case mode
    0 (or (get-in context [:mem i]) 0)
    1 i
    2 (or (+ (get-in context [:mem i])
             (:base context)) 0)
    (throw (Exception. (str "Unknown memory access mode: " mode)))))

(defn mem
  ([context mode i]
   (let [ptr (mem-ptr context mode i)]
     (when (or (nil? ptr)
               (neg? ptr)) (throw (Exception. (str "Negative/nil address reference: " ptr))))
     (or (get-in context [:mem ptr])
         0)))
  ([context mode i value]
   (let [ptr (mem-ptr context mode i)]
     (when (or (nil? ptr)
               (neg? ptr)) (throw (Exception. (str "Negative/nil address reference: " ptr))))
     (assoc-in context [:mem ptr] value))))

(defn input [{:keys [ip] :as context} mode]
  (-> (mem context mode (inc ip)
           (a/<!! (:in context)))
      (assoc :ip (+ ip 2))))

(defn output [{:keys [ip] :as context} mode]
  (a/>!! (:out context)
         (mem context mode (inc ip)))
  (assoc context :ip (+ ip 2)))

(defn math [{:keys [ip] :as context} op-fn c b a]
  (-> (mem context a (+ ip 3)
           (op-fn (mem context b (+ ip 2))
                  (mem context c (+ ip 1))))
      (assoc :ip (+ ip 4))))

(defn jump [{:keys [ip] :as context} test-fn c b]
  (if (test-fn (mem context c (inc ip)))
    (assoc context :ip (mem context b (+ ip 2)))
    (assoc context :ip (+ ip 3))))

(defn cmp [{:keys [ip] :as context} cmp-fn c b a]
  (-> (if (cmp-fn (mem context c (inc ip))
                  (mem context b (+ ip 2)))
        (mem context a (+ ip 3) 1)
        (mem context a (+ ip 3) 0))
      (assoc :ip (+ ip 4))))

(defn run
  ([{:keys [ip] :as context}]
   (let [{:keys [a b c de]} (parse-intcode (mem context 1 ip))]
     (condp = de
       1 (math context + c b a)
       2 (math context * c b a)
       3 (input context c)
       4 (output context c)
       5 (jump context (comp not zero?) c b)
       6 (jump context zero? c b)
       7 (cmp context < c b a)
       8 (cmp context = c b a)
       9 (assoc context
                :base (+ (:base context)
                         (mem context c (inc ip)))
                :ip (+ ip 2))
       99 (do (a/close! (:in context))
              (a/close! (:out context)))
       (throw (Exception. (str "Unknown Intcode: " de)))))))

(defn make-runner [context]
  (fn []
    (->> context (iterate run) (take-while some?) last)))

(defn solve [program input]
  (let [in (a/chan)
        out (a/chan)
        runner (make-runner {:mem program :ip 0 :in in :out out :base 0})]
    (a/go (a/>! in input))
    (a/thread (runner))
    (first (take-while some? (repeatedly #(a/<!! out))))))

(defn -main []
  (let [program (-> "resources/day9-input.txt" slurp parse-program)]
    (println "part 1:" (solve program 1))
    (println "part 2:" (solve program 2))))

(comment
  (let [mem (-> "resources/day9-input.txt" slurp parse-program)]
    (solve mem 1))
  ;; => 3063082071
  (let [mem (-> "resources/day9-input.txt" slurp parse-program)]
    (solve mem 2))
  ;; => 81348  
  )
