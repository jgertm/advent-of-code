(ns advent-of-code.no-time-for-a-taxicab
  (:require [clojure.java.io :as io]))

(def ^:private instruction-str
  (-> "input-01.txt" io/resource slurp))

(def ^:private headings [:N :E :S :W])
(def ^:private directions [:L :R])

(defn- parse-instructions
  "Extract the individual turn and step instructions from a string, returning a
  seq of 2-vectors of keywordized turn direction and integer step amount."
  [instruction-str]
  (->> instruction-str
       (re-seq #"([LR])(\d+)")
       (map (fn [[_ direction steps]]
              [(keyword direction) (Integer/parseInt steps)]))))

(defn- turn
  "Determines the new heading from the current heading and a turn direction."
  [heading direction]
  {:pre [(contains? (set headings) heading)
         (contains? (set directions) direction)]}
  (let [left (zipmap (next (cycle headings)) headings)
        right (zipmap headings (next (cycle headings)))]
    (get-in {:L left, :R right} [direction heading])))

(defn- inertialize-instructions
  "Transforms a sequence of instructions from the body-centered, turn-based
  coordinate system into a geodetic coordinate system."
  [instruction-seq]
  {:post [#(= (count instruction-seq) (count %))]}
  (let [directions (map first instruction-seq)
        steps (map second instruction-seq)
        headings (next (reductions turn :N directions))]
    (map vector headings steps)))

(defn- move
  ([]
   [0 0])
  ([instruction]
   (move (move) instruction))
  ([[lat lon] [heading steps]]
   (case heading
     :N [(+ lat steps) lon]
     :S [(- lat steps) lon]
     :E [lat (+ lon steps)]
     :W [lat (- lon steps)])))

(defn- final-location
  "Computes the final location from a sequence of absolute movement instruction."
  [instruction-seq]
  {:pre [(every? (every-pred #(= 2 (count %))
                             #(contains? (set headings) (first %))
                             #(pos? (second %)))
                 instruction-seq)]
   :post [#(= 2 (count %))]}
  (reduce move [0 0] instruction-seq))

(defn- taxicab-metric
  "Computes the taxicab (or Manhattan) distance of two points in Cartesian
  coordinates."
  ([p]
   (->> p
        (map #(Math/abs %))
        (reduce +)))
  ([pa pb]
   (->> pb
        (map (fn [ca cb] (Math/abs (- ca cb))) pa)
        (reduce +))))

(defn- solve-part-one
  "Determines the shortest path to Easter Bunny HQ, given a file containing
  turn-by-turn instructions to the HQ."
  [instruction-str]
  (-> instruction-str
      parse-instructions
      inertialize-instructions
      final-location
      taxicab-metric))

(comment
  (solve-part-one instructions))

(defn- expand-instructions
  "Replaces every multi-step absolute instruction by respectively many single
  steps."
  [instruction-seq]
  (mapcat (fn [[heading steps]] (repeat steps [heading 1])) instruction-seq))

(defn- intermediate-locations
  "Computes the sequence of locations that arise from following a sequence of
  inertial instructions. "
  [instruction-seq]
  (reductions move [0 0] instruction-seq))

(defn- first-duplicate
  "Returns the first element from a sequence that has already occurred before."
  [xs]
  {:pre [(sequential? xs)]}
  (reduce (fn [seen new]
            (if-not (contains? seen new)
              (conj seen new)
              (reduced new)))
          #{} xs))

(defn- solve-part-two
  [instruction-str]
  (-> instruction-str
      parse-instructions
      inertialize-instructions
      expand-instructions
      intermediate-locations
      first-duplicate
      taxicab-metric))

(comment
  (solve-part-two instructions))

(defn present-solution
  [input-file]
  (let [instruction-str (slurp input-file)]
    (println (format "Final intersection is %d blocks away." (solve-part-one instruction-str)))
    (println (format "First intersection to be visited twice is %d blocks away." (solve-part-two instruction-str)))))
