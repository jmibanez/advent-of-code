(require '[clojure.string :as str])
(require '[clojure.walk :as w])
(require '[clojure.zip :as z])

(defn engine-part? [cell]
  (and (not (= \. cell))
       (not (Character/isDigit cell))))

(defn gear? [cell]
  (= \* cell))

(defn get-chunk-start [grid i j]
  (loop [digit-start (- j 1)]
    (if (< digit-start 0)
      0
      (if (not (Character/isDigit (aget grid i digit-start)))
        (+ digit-start 1)
        (recur (- digit-start 1))))))

(defn ->chunk [row col chunk-str]
  {:row row :col col
   :chunk (Integer/parseInt chunk-str)})

(defn get-chunk [grid i j]
  (let [max-height (alength grid)
        max-width (alength (aget grid 0))]
    (cond
      (< i 0) nil
      (< j 0) nil
      (>= i max-height) nil
      (>= j max-width) nil
      :else
      (let [cell (aget grid i j)]
        (if (Character/isDigit cell)
          (let [digit-start (get-chunk-start grid i j)]
            (loop [next digit-start
                   result ""]
              (if (< next max-width)
                (let [next-cell (aget grid i next)]
                  (if (Character/isDigit next-cell)
                    (recur (+ next 1)
                           (str result next-cell))
                    (->chunk i digit-start result)))
                (->chunk i digit-start result)))))))))

(defn grid-neighbors [grid i j]
  (let [height (alength grid)
        width (alength (aget grid 0))
        neighbors (distinct
                   (for [row-offset (range -1 2)
                         col-offset (range -1 2)]
                     (get-chunk grid
                                (+ i row-offset)
                                (+ j col-offset))))]
    (filter some?
            neighbors)))

(defn read-grid [input-name]
  (let [lines (-> input-name
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (->> lines
         (map seq)
         to-array-2d)))

(def grid (read-grid "3.txt"))

(def ans-1
  (->> (for [[i row]  (map-indexed list grid)
             [j cell] (map-indexed list row)
             :when (engine-part? cell)]
         (grid-neighbors grid i j))
       distinct
       flatten
       (map :chunk)
       (reduce +)))

(def ans-2
  (->> (for [[i row]  (map-indexed list grid)
             [j cell] (map-indexed list row)
             :when (gear? cell)]
         (let [neighbors (grid-neighbors grid i j)]
           (if (= (count neighbors) 2)
             (->> neighbors
                  (map :chunk)
                  (reduce *))
             'nil)))
       (filter some?)
       flatten
       (reduce +)))
