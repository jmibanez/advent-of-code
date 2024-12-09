(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])

(def search-string #"XMAS")

(defn read-grid [input]
  (map into-array
       (u/read-file-as-lines input)))

(defn count-stride-match [stride]
  (let [str-stride (apply str stride)
        reverse-str-stride (apply str (reverse stride))]
    (+ (count (re-seq search-string reverse-str-stride))
       (count (re-seq search-string str-stride)))))

(defn count-matching-rows [grid]
  (reduce +
          (map count-stride-match grid)))


(defn count-matching-cols [grid]
  (reduce +
          (map count-stride-match
               (apply map vector grid))))

(defn ->diagonal [strides stride]
  (loop [stride stride
         col 0
         out []]
    (if (< stride 0)
      (filter #(not (false? %)) out)
      (let [value (nth (nth strides stride []) col false)]
        (recur (dec stride) (inc col) (conj out value))))))

(defn grid-diagonals [grid]
  (let [length         (count grid)
        height         (count (first grid))
        diagonal-count (dec (+ length height))]
    (->> (range diagonal-count)
         (map #(->diagonal grid %)))))

(defn count-matching-diags [grid]
  (let [left-diags (grid-diagonals grid)
        right-diags (grid-diagonals (reverse (apply map vector grid)))]
    (+
     (reduce + (map count-stride-match left-diags))
     (reduce + (map count-stride-match right-diags)))))

(defn count-matches-in-grid [grid]
  (+ (count-matching-rows grid)
     (count-matching-cols grid)
     (count-matching-diags grid)))


(defn get-check-spots-at-cell [grid cell i j]
  (let [tl (nth (nth grid (- i 1))
                (- j 1))
        tr (nth (nth grid (- i 1))
                (+ j 1))
        bl (nth (nth grid (+ i 1))
                (- j 1))
        br (nth (nth grid (+ i 1))
                (+ j 1))]
      [i j tl tr cell bl br]))

(defn cell-is-x-mas-centered? [grid cell i j]
  (let [[_ _ tl tr _ bl br]
        (get-check-spots-at-cell grid cell i j)]
    (and
     (= \A cell)
     (or
      (and
       (= \M tl)
       (= \S br))
      (and
       (= \S tl)
       (= \M br)))
     (or
      (and
       (= \M bl)
       (= \S tr))
      (and
       (= \S bl)
       (= \M tr))))))

(defn search-x-mas [grid]
  (let [length (count grid)
        height (count (first grid))]
    (for [[i row] (map-indexed list grid)
          [j cell] (map-indexed list row)
          :when (and (>= i 1)
                     (>= j 1)
                     (<= i (- length 2))
                     (<= j (- height 2)))]

      (cell-is-x-mas-centered? grid cell i j))))

;; (def ans (count (filter true? (search-x-mas (read-grid "aoc4-input.txt")))))
