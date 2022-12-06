(ns advent.x3
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set])

(def parse (str/split (slurp "../inputs/3") #"\n"))

(defn halve [s] (split-at (/ (count s) 2) s))

(defn priority [c]
  (cond (Character/isUpperCase c) (- (int c) 38)
        :else (- (int c) 96)))

(def part-one
  (->> parse
       (map halve)
       (map #(map set %))
       (map #(apply clojure.set/intersection %))
       (map vec)
       flatten
       (map priority)
       (apply +)))

(def part-two
  (->> parse
       (partition 3)
       (map #(map set %))
       (map #(apply clojure.set/intersection %))
       (map vec)
       flatten
       (map priority)
       (apply +)))

(defn -main [& args]
  (println part-one)
  (println part-two))
