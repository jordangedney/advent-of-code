(ns advent.x1
  (:gen-class))

(require '[clojure.string :as str])

(def parse
  (->> (str/split (slurp "../inputs/1") #"\n\n")
       (map #(str/split %1 #"\n"))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (map #(apply + %))
       ))

(def part-one (reduce max parse))
(def part-two (->> parse
               sort
               reverse
               (take 3)
               (apply +)))

(defn -main [& args]
  (println part-one)
  (println part-two))
