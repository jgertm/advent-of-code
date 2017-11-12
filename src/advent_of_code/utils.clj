(ns advent-of-code.utils)

(defn index-by
  [f xrel]
  {:pre [(coll? xrel) (every? map? xrel)]
   :post [map?]}
  (->> xrel
       (map (juxt f identity))
       (into {})))

(def conjv
  (fnil conj []))

(def conjs
  (fnil conj #{}))
