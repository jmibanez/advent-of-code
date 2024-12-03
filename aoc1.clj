(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])


(defn to-int [s]
  (Integer/parseInt s))

(defn delta [[a b]]
  (abs (- a b)))

(defn get-distance-of-lists [input]
  (let [lines (u/read-file-as-lines input)
        tuple-lists (map #(->> %
                               (map to-int))
                         (apply map vector
                                (for [l lines]
                                  (str/split l #" +"))))
        left-list (-> tuple-lists first sort)
        right-list (-> tuple-lists last sort)
        sorted-tuple-lists (vector left-list right-list)]
    (reduce +
            (map delta (apply map vector sorted-tuple-lists)))))


(get-distance-of-lists "aoc1-example.txt")

(def ans (get-distance-of-lists "aoc1-input.txt"))

