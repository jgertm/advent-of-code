(ns advent-of-code.security-through-obscurity
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private input-file
  (io/resource "input-04.txt"))

(defn- parse-room
  "Takes a triple of strings representing the components of a room and massages
  them into a more useful map:
  - removes dashes from `name`
  - parses `sector-id`to int
  - checksum remains as is"
  [[name sector-id checksum]]
  {:name (str/replace name #"-" "")
   :sector-id (Integer/parseInt sector-id)
   :checksum checksum})

(defn- parse-rooms
  "Takes the problem input string and returns a sequence of maps, each
  representing a room."
  [rooms-str]
  (->> rooms-str
       (re-seq #"([a-z-]+)-(\d+)\[([a-z]{5})\]")
       (map (comp parse-room next))))

(defn- room-name-checksum
  "Computes the checksum of a room name, given by the five most frequent
  characters, with ties broken by alphabetic position."
  [name]
  (->> name
       frequencies
       (sort-by (juxt second (comp - int first)) (comp - compare))
       (take 5)
       keys
       str/join))

(defn- real-room?
  "Predicate to verify if a rooms stated checksum matches its names' checksum."
  [{:keys [name checksum]}]
  (= checksum (room-name-checksum name)))

(defn- solve-part-one
  "Computes the sum of all the real rooms' sector IDs in the input string."
  [rooms-str]
  (->> rooms-str
       parse-rooms
       (filter real-room?)
       (map :sector-id)
       (reduce +)))

(defn present-solution
  [input-file]
  (let [rooms-string (slurp input-file)]
    (println (format "Sum of all real rooms' sector IDs: %d." (solve-part-one rooms-string)))))
