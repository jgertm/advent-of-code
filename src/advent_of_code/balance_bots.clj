(ns advent-of-code.balance-bots
  (:require [advent-of-code.utils :as utils]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def ^:private instructions-str
  (-> "input-10.txt" io/resource slurp))

(def ^:private initial-regex
  #"value (\d+) goes to bot (\d+)")

(defn- extract-initial-state
  [instructions-str]
  "Parses the initial bot loadings to determine the first simulation state from
  the problem input string."
  {:bot (->> instructions-str
        (re-seq initial-regex)
        (map (fn [[_ value robot]] {:robot (Integer/parseInt robot)
                                    :value (Integer/parseInt value)}))
        (reduce (fn [acc {:keys [robot value]}]
                  (update acc robot utils/conjs value)) {}))
   :output {}})

(def ^:private transfer-regex
  #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")

(defn- extract-transfer-rules
  "Parses the rules that govern the transfer of chips from fully-laden bots from
  the problem input string."
  [instructions-str]
  (->> instructions-str
       (re-seq transfer-regex)
       (map (fn [[_ robot lo-ent lo-id hi-ent hi-id]]
              {:robot (Integer/parseInt robot)
               :low [(keyword lo-ent) (Integer/parseInt lo-id)]
               :high [(keyword hi-ent) (Integer/parseInt hi-id)]}))
       (utils/index-by :robot)))

(defn- simulate-step
  "Applies a set of rules to a simulation state to produce a new state."
  [state rules]
  (letfn [(active-robot? [[_ values]] (= 2 (count values)))
          (act-robotically [state-acc [robot values]]
            (let [rule (get rules robot)
                  [lo hi] (sort values)]
              (-> state-acc
                  (update :bot dissoc robot)
                  (update-in (:low rule) utils/conjs lo)
                  (update-in (:high rule) utils/conjs hi))))]
    (->> state
         :bot
         (filter active-robot?)
         (reduce act-robotically state))))

(defn- simulate
  "Executes a simulation, as defined by an inital state and a step function until
  the state remains fixed."
  [initial-state rules]
  (->> initial-state
       (iterate #(simulate-step % rules))
       (reduce (fn [acc step]
                 (if (= (last acc) step)
                   (reduced acc)
                   (conj acc step))) [])))

(defn- bot-inventories
  "Shows the distinct inventories for every bot over the course of a simulation."
  [simulation-trace]
  {:pre [(sequential? simulation-trace) (every? map? simulation-trace)]}
  (let [bots (reduce set/union (map (comp set keys :bot) simulation-trace))]
    (->> simulation-trace
         (map :bot)
         (reduce (partial merge-with utils/conjs) (zipmap bots (repeat #{})) ))))

(defn- find-special-bot
  "Returns the ID of the bot that held a two given chips at once from a summary
  map of every bots' inventories."
  [chip-combo inventory-summary]
  {:pre [(map? inventory-summary) (every? set? (vals inventory-summary)) (= 2 (count chip-combo))]}
  (let [special-bots (filter (fn [[_ inventories]] (contains? inventories (set chip-combo))) inventory-summary)]
    (assert (= 1 (count special-bots)))
    (key (first special-bots))))

(defn- solve-part-one
  "Determines the bot ID that had to compare chips 61 and 17."
  [instructions-str]
  (let [state (extract-initial-state instructions-str)
        rules (extract-transfer-rules instructions-str)]
    (->> (simulate state rules)
         bot-inventories
         (find-special-bot [61 17]))))

(assert (= 98 (solve-part-one instructions-str)))

(defn- output-bin-contents
  "Returns the chip IDs of a set of output bins in a given state."
  [state bins]
  (map first (-> state
                 :output
                 (select-keys bins)
                 vals)))

(defn- solve-part-two
  "Computes the product of the chip IDs that are contained in the bins 0, 1 and 2
  at the end of the simulation."
  [instructions-str]
  (let [state (extract-initial-state instructions-str)
        rules (extract-transfer-rules instructions-str)]
    (reduce * (-> (simulate state rules)
                  last
                  (output-bin-contents [0 1 2])))))

(assert (= 4042 (solve-part-two instructions-str)))

(defn present-solution
  [input-file]
  (let [instructions-string (slurp input-file)]
    (println (format "Bot that compared chips 61 and 17: %d" (solve-part-one instructions-string)))
    (println (format "Product of chip IDs of bin 0, 1 and 2: %d" (solve-part-two instructions-string)))))
