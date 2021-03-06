(ns advent-of-code.core
  (:gen-class)
  (:require [advent-of-code.balance-bots :as day10]
            [advent-of-code.no-time-for-a-taxicab :as day1]
            advent-of-code.no-time-for-a-taxicab-test
            [advent-of-code.security-through-obscurity :as day4]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.test :as test]))

(def ^:private solved-days #{1 4 10})

(def ^:private cli-options
  [["-d" "--day DAY" "Day number"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(contains? solved-days %)]]
   ["-i" "--input INPUT-FILE" "File containing the problem input"
    :default (io/resource "input-01.txt")
    :parse-fn io/file]
   ["-t" "--test" "Run the test suite"
    :default false]
   ["-h" "--help" "Print the help message"
    :default nil]])

(def ^:private help-text
  (str "Welcome to my solution to Advent of Code 2016.
  Usage: aoc (--help|--test|--day <day#> --input <input-file>)\n\n"
       (->> cli-options
            (map #(->> % (take 3) (str/join " ") (format "  %s")))
            (str/join "\n"))))

(defn -main [& args]
  (let [{:keys [options errors]} (parse-opts args cli-options)
        {:keys [day input]} options]
    (cond
      (:help options) (println help-text)
      (:test options) (test/run-all-tests #"^advent-of-code\..*test$")
      (and day input) (case day
                        1 (day1/present-solution input)
                        4 (day4/present-solution input)
                        10 (day10/present-solution input))
      :else (println "Sorry, I didn't understand that. Have you considered running me with --help?"))))
