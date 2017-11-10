(ns advent-of-code.core
  (:gen-class)
  (:require [advent-of-code.no-time-for-a-taxicab :as day1]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]))

(def ^:private solved-days #{1})

(def ^:private cli-options
  [["-d" "--day DAY" "Day number"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(contains? solved-days %)]]
   ["-i" "--input INPUT-FILE" "File containing the problem input"
    :default (io/resource "input-01.txt")
    :parse-fn io/file]
   ["-h" "--help" "Print the help message"
    :default nil]])

(def ^:private help-text
  (str "Welcome to my solution to Advent of Code 2016.
  Usage: aoc -d <day#> -i <input-file>\n\n"
       (->> cli-options
            (map #(->> % (take 3) (str/join " ") (format "  %s")))
            (str/join "\n"))))

(defn -main [& args]
  (let [{:keys [options errors]} (parse-opts args cli-options)
        {:keys [day input]} options]
    (cond
      (:help options) (println help-text)
      (and day input) (case day
                        1 (day1/present-solution input))
      :else (println "Sorry, I didn't understand that. Have you considered running me with --help?"))))
