(ns advent-of-code.security-through-obscurity
  (:require [advent-of-code.utils :as utils]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private rooms-str
  (-> "input-04.txt" io/resource slurp))

(defn- parse-room
  "Takes a triple of strings representing the components of a room and massages
  them into a more useful map:
  - name remains as is
  - parses `sector-id`to int
  - checksum remains as is"
  [[name sector-id checksum]]
  {:name name
   :sector-id (Integer/parseInt sector-id)
   :checksum checksum})

(defn- parse-rooms
  "Takes the problem input string and returns a sequence of maps, each
  representing a room."
  [rooms-str]
  (->> rooms-str
       (re-seq #"([a-z-]+)-(\d+)\[([a-z]{5})\]")
       (map #(-> % next parse-room))))

(defn- room-name-checksum
  "Computes the checksum of a room name, given by the five most frequent
  characters, with ties broken by alphabetic position."
  [name]
  (->> (str/replace name #"-" "")
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

(comment
  (solve-part-one rooms-str))

(defn- shift-char
  [c offset]
  (let [char-pos (- (int c) (int \a))
        total-offset (mod (+ char-pos offset) 26)]
    (char (+ (int \a) total-offset))))

(defn- shift-string
  [str offset]
  (->> str
       (map #(cond-> %
              (<= (int \a) (int %) (int \z))
              (shift-char offset)))
       str/join))

(defn- decrypt-room
  [{:keys [sector-id] :as room}]
  (update room :name shift-string sector-id))

(defn- solve-part-two
  [rooms-str]
  (get-in (->> rooms-str
        parse-rooms
        (filter real-room?)
        (map decrypt-room)
        (utils/index-by :name))
          ["northpole-object-storage" :sector-id]))

(comment
  (solve-part-two rooms-str))

(defn present-solution
  [input-file]
  (let [rooms-string (slurp input-file)]
    (println (format "Sum of all real rooms' sector IDs: %d." (solve-part-one rooms-string)))
    (println (format "Northpole object storage room sector ID: %s" (solve-part-two rooms-string)))))
