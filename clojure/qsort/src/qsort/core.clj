(ns qsort.core
  (:gen-class))
(require '[clojure.core.match :refer [match]])

(defn qsort [v]
  (match [v]
     [[]] []
     [[x & xs]] (let [bigger (vec (filter #(>= % x) xs))
                      smaller (vec (filter #(< % x) xs))]
                 (vec (concat (conj (qsort smaller) x) (qsort bigger))))))

(defn -main [& args]
  (println (qsort [5 4 1 3 2])))
