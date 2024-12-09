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
