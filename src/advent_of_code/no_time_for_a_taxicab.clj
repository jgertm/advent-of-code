(ns advent-of-code.no-time-for-a-taxicab
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(def ^:private instruction-str
  (-> "input-01.txt" io/resource slurp))

;; This keeps generative testing durations down
(s/def ::int (s/int-in (- 10000) 10000))
(s/def ::nat (s/int-in 0 10000))

(def ^:private headings [:N :E :S :W])
(s/def ::heading (set headings))

(def ^:private directions [:L :R])
(s/def ::direction (set directions))

(s/def ::turn-instruction
  (s/tuple ::direction ::nat))

(s/def ::head-instruction
  (s/tuple ::heading ::nat))

(s/def ::position
  (s/tuple ::int ::int))

(defn parse-instructions
  "Extract the individual turn and step instructions from a string, returning a
  seq of 2-vectors of keywordized turn direction and integer step amount."
  [instruction-str]
  (->> instruction-str
       (re-seq #"([LR])(\d+)")
       (map (fn [[_ direction steps]]
              [(keyword direction) (Integer/parseInt steps)]))))

(s/fdef parse-instructions
        :args string?
        :ret (s/coll-of ::turn-instruction))

(defn turn
  "Determines the new heading from the current heading and a turn direction."
  [heading direction]
  (let [left (zipmap (next (cycle headings)) headings)
        right (zipmap headings (next (cycle headings)))]
    (get-in {:L left, :R right} [direction heading])))

(s/fdef turn
        :args (s/cat :heading ::heading :direction ::direction)
        :ret ::heading)

(defn inertialize-instructions
  "Transforms a sequence of instructions from the body-centered, turn-based
  coordinate system into a geodetic coordinate system."
  [turn-instruction-seq]
  (let [directions (map first turn-instruction-seq)
        steps (map second turn-instruction-seq)
        headings (next (reductions turn :N directions))]
    (map vector headings steps)))

(s/fdef inertialize-instructions
        :args (s/coll-of ::turn-instruction)
        :ret (s/coll-of ::head-instruction))

(defn move
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

(s/fdef move
        :args (s/cat :position ::position :instruction ::head-instruction)
        :ret ::position)

(defn final-location
  "Computes the final location from a sequence of absolute movement instruction."
  [instruction-seq]
  (reduce move [0 0] instruction-seq))

(s/fdef final-location
        :args (s/coll-of ::head-instruction)
        :ret ::position)

(defn taxicab-metric
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

(s/fdef taxicab-metric
        :args (s/tuple ::position)
        :ret (s/and integer? (s/alt :pos pos? :zero zero?)))

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

(defn expand-instructions
  "Replaces every multi-step absolute instruction by respectively many single
  steps."
  [instruction-seq]
  (mapcat (fn [[heading steps]] (repeat steps [heading 1])) instruction-seq))

(s/fdef expand-instructions
        :args (s/coll-of ::head-instruction)
        :ret (s/coll-of ::head-instruction))

(defn intermediate-locations
  "Computes the sequence of locations that arise from following a sequence of
  inertial instructions. "
  [instruction-seq]
  (reductions move [0 0] instruction-seq))

(s/fdef intermediate-locations
        :args (s/coll-of ::head-instruction)
        :ret (s/coll-of ::position))

(defn first-duplicate
  "Returns the first element from a sequence that has already occurred before."
  [xs]
  (reduce (fn [seen new]
            (if-not (contains? seen new)
              (conj seen new)
              (reduced new)))
          #{} xs))

(s/fdef first-duplicate
        :args sequential?)

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
