(ns advent-of-code.no-time-for-a-taxicab-test
  (:require [advent-of-code.no-time-for-a-taxicab :as p]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defspec parse-instructions-preserves-elements
  (prop/for-all [turn-instructions (sgen/vector (s/gen ::p/turn-instruction))]
                (let [serialized-instructions (->> turn-instructions
                                                   (map (fn [[dir n]] (str (name dir) (str n))))
                                                   (str/join ", "))
                      deserialized-instructions (p/parse-instructions serialized-instructions)]
                  (= turn-instructions deserialized-instructions))))

(defspec turn-is-cyclic
  (prop/for-all [heading (s/gen ::p/heading)
                 direction (s/gen ::p/direction)
                 n (gen/fmap (partial * 4) gen/pos-int)]
                (= heading
                   (nth (iterate #(p/turn % direction) heading) n))))

(defspec turn-is-commutative
  (prop/for-all [heading (s/gen ::p/heading)
                 first-direction (s/gen ::p/direction)
                 second-direction (s/gen ::p/direction)]
                (= (-> heading
                       (p/turn first-direction)
                       (p/turn second-direction))
                   (-> heading
                       (p/turn second-direction)
                       (p/turn first-direction)))))

(defspec move-moves-along-basis-vectors
  (prop/for-all [position (s/gen ::p/position)
                 instruction (s/gen ::p/head-instruction)]
                (not-empty (set/intersection (set position)
                                             (set (p/move position instruction))))))

(defspec move-is-commutative
  (prop/for-all [position (s/gen ::p/position)
                 first-instruction (s/gen ::p/head-instruction)
                 second-instruction (s/gen ::p/head-instruction)]
                (= (-> position
                       (p/move first-instruction)
                       (p/move second-instruction))
                   (-> position
                       (p/move second-instruction)
                       (p/move first-instruction)))))

(defspec taxicab-metric-obeys-non-negativity-condition
  (prop/for-all [pa (s/gen ::p/position)
                 pb (s/gen ::p/position)]
                (<= 0 (p/taxicab-metric pa pb))))

(defspec taxicab-metric-obeys-identity-of-indiscernibles
  (prop/for-all [p (s/gen ::p/position)]
                (= 0 (p/taxicab-metric p p))))

(defspec taxicab-metric-is-symmetric
  (prop/for-all [pa (s/gen ::p/position)
                 pb (s/gen ::p/position)]
                (= (p/taxicab-metric pa pb)
                   (p/taxicab-metric pb pa))))

(defspec taxicab-metric-obeys-triangle-inequality
  (prop/for-all [pa (s/gen ::p/position)
                 pb (s/gen ::p/position)
                 pc (s/gen ::p/position)]
                (<= (p/taxicab-metric pa pc)
                    (+ (p/taxicab-metric pa pb)
                       (p/taxicab-metric pb pc)))))

(defspec taxicab-metric-is-translation-invariant
  (prop/for-all [pa (s/gen ::p/position)
                 pb (s/gen ::p/position)
                 offset gen/int]
                (= (p/taxicab-metric pa pb)
                   (p/taxicab-metric (mapv (partial + offset) pa)
                                     (mapv (partial + offset) pb)))))

(defspec expand-instruction-preserves-heading
  (prop/for-all [instruction (s/gen ::p/head-instruction)]
                (let [[heading _] instruction]
                  (every? #(= heading (first %)) (p/expand-instruction instruction)))))

(defspec expand-instruction-observes-cardinality
  (prop/for-all [instruction (s/gen ::p/head-instruction)]
                (let [[_ steps] instruction]
                  (= steps (count (p/expand-instruction instruction))))))

(defspec first-duplicate-finds-duplicate-when-present
  (prop/for-all [elem gen/simple-type]
                (= elem (p/first-duplicate (repeat elem)))))

(defspec first-duplicate-finds-nothing-when-all-distinct
  (prop/for-all [v (gen/vector-distinct gen/simple-type)]
               (nil? (p/first-duplicate v))))
