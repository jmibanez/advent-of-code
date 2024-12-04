(require '[clojure.math :as m :use signum])
(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])
(require '[clojure.set :as st])

(defn to-int [s]
  (Integer/parseInt s))

(defn report-as-int [report-line]
  (map to-int report-line))

(defn get-reports [input]
  (let [lines (u/read-file-as-lines input)]
    (map report-as-int
         (map #(str/split % #" +")
              lines))))


(defn is-opposite-sign? [a b]
  (let [asignum (signum a)
        bsignum (signum b)
        pair [asignum bsignum]]

    (or (= [-1.0 1.0] pair)
        (= [1.0 -1.0] pair))))

(defn is-safe-delta? [prev-delta new-delta]
  (if (nil? prev-delta)
    true
    (and (not (is-opposite-sign? prev-delta new-delta))
         (>= (abs prev-delta) 1)
         (<= (abs prev-delta) 3)
         (>= (abs new-delta) 1)
         (<= (abs new-delta) 3))))

(defn element-deltas [list]
  (map - list (rest list)))

(defn is-report-safe? [report]
  (let [report-deltas (element-deltas report)
        is-safe-deltas? (map is-safe-delta?
                             report-deltas
                             (rest report-deltas))]
    (reduce #(and %1 %2) true is-safe-deltas?)))

(defn get-safe-reports [reports]
  (filter is-report-safe? reports))


(defn drop-nth [n coll]
   (keep-indexed #(if (not= %1 n) %2) coll))

(defn can-tolerate? [report]
  (reduce #(or %1 %2)
          (for [i (range (count report))]
            (is-report-safe? (drop-nth i report)))))

(defn get-tolerable-reports [reports]
  (let [unsafe-reports (filter #(not (is-report-safe? %)) reports)]
    (filter can-tolerate? unsafe-reports)))


(defn count-safe-reports [input]
  (let [reports (get-reports input)
        sure-safe-reports (get-safe-reports reports)
        tolerable-reports (get-tolerable-reports reports)]
    (+
     (count sure-safe-reports)
     (count tolerable-reports))))
