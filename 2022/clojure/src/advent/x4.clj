(ns advent.x4
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.set])

(def parse
  (->> (str/split (slurp "../inputs/4") #"\n")
        (map #(str/split %1 #","))
        (map (fn [xs] (apply concat (map #(str/split % #"-") xs))))
        (map (fn [xs] (map read-string xs)))))

(defn contains [x y a b]
  (or (and (<= x a) (>= y b))
      (and (<= a x) (>= b y))))

(def part-one
  (get (->> parse
            (map #(apply contains %))
            frequencies)
       true))

(defn any-elem? [x y a b]
  (empty?
    (clojure.set/intersection
     (set (range x (+ 1 y)))
     (set (range a (+ 1 b))))))

(def part-two
  (get (->> parse
            (map #(apply any-elem? %))
            frequencies)
  false))

(defn -main [& args]
  (println part-one)
  (println part-two))
