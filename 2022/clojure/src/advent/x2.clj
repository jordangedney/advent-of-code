(ns advent.x2
  (:gen-class))

(require '[clojure.string :as str])

(def parse
  (->> (str/split (slurp "../inputs/2") #"\n")
       (map #(str/split %1 #" "))))

(defn score-shape [[x y]]
  (case y
    "X" 1
    "Y" 2
    "Z" 3))

(defn won [round]
  (case round
    ["A" "Y"] true
    ["B" "Z"] true
    ["C" "X"] true
    false))

(defn draw [round]
  (case round
    ["A" "X"] true
    ["B" "Y"] true
    ["C" "Z"] true
    false))

(defn score-round [round]
  (cond
    (won round) 6
    (draw round) 3
    :else 0))

(def part-one
  (->> parse
       (map #(+ (score-shape %) (score-round %)))
       (apply +)))

(defn get-move [x y]
  (case y
    "X" (case x
          "A" "Z"
          "B" "X"
          "C" "Y")
    "Y" (case x
          "A" "X"
          "B" "Y"
          "C" "Z")
    "Z" (case x
          "A" "Y"
          "B" "Z"
          "C" "X")))

(def part-two
  (->> parse
       (map (fn [[x y]] [x (get-move x y)]))
       (map #(+ (score-shape %) (score-round %)))
       (apply +)))

(defn -main [& args]
  (println part-one)
  (println part-two))
