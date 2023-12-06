(require '[clojure.string :as str])
(require '[util :as u])

(defn seeds-to-plant [seeds-def]
  (let [[_ seed-id-str] (str/split seeds-def #":")]
    (str/split (str/trim seed-id-str)
               #"( )+")))

(defn map-over-range-inner [^long a ^long b ^long n]
  (for [i (range n)]
    [(str (+ a i)) (str (+ b i))]))

(defn map-over-range [src-id dest-id n]
  (let [a (Long/parseLong src-id)
        b (Long/parseLong dest-id)]
    (map-over-range-inner a b (Long/parseLong n))))

(defn expand-span [span]
  (let [[start n] (map #(Long/parseLong %) span)]
    (map str (take n (iterate inc start)))))

(defn ->sparse-mapping [mapping]
  (let [[dest-id src-id n] (str/split mapping #"( )+")]
    [(Long/parseLong src-id)
     (Long/parseLong dest-id)
     (Long/parseLong n)]))

(defn handle-mapping [current-mappings id-mappings]
  (let [[mapping-type _] (str/split (first id-mappings)
                                    #"( )+")
        [source-type _ dest-type] (->> (str/split mapping-type
                                                  #"-")
                                       (map str/trim))
        mappings (rest id-mappings)
        mapping-map (get current-mappings source-type)]

    (assoc current-mappings source-type
           (map ->sparse-mapping mappings))))

(defn match-sparse-mapping? [^long id entry]
  (let [[a b n] entry]
    (if (and
         (<= id (+ a n))
         (<= a id))
      entry)))

(defn transform-id [transform-map src id]
  (let [mapping (get transform-map src)
        id-val (Long/parseLong id)
        matching-entry (some (partial match-sparse-mapping? id-val) mapping)]

    (if matching-entry
      (let [[a b n] matching-entry
            offset (- id-val a)]
        (str (+ b offset)))
      id)))

(defn find-location [transform-map seed-id]
  (->> seed-id
       (transform-id transform-map "seed")
       (transform-id transform-map "soil")
       (transform-id transform-map "fertilizer")
       (transform-id transform-map "water")
       (transform-id transform-map "light")
       (transform-id transform-map "temperature")
       (transform-id transform-map "humidity")))

;; (defn find-location-spans [transform-map span]
;;   (->> span
;;        (transform-id-span transform-map "seed")
;;        (transform-id-span transform-map "soil")
;;        (transform-id-span transform-map "fertilizer")
;;        (transform-id-span transform-map "water")
;;        (transform-id-span transform-map "light")
;;        (transform-id-span transform-map "temperature")
;;        (transform-id-span transform-map "humidity")))

(defn find-lowest-location [seeds transform-map]
  (let [locs (map (partial find-location transform-map) seeds)]
    (apply min (map #(Long/parseLong %) (sort locs)))))

;; (defn find-lowest-location-spans [seeds transform-map]
;;   (let [locs (map (partial find-location-spans transform-map) seeds)]
;;     (apply min (map #(Long/parseLong %) (sort locs)))))


(def initial-map {"seed" {}
                  "soil" {}
                  "fertilizer" {}
                  "water" {}
                  "light" {}
                  "temperature" {}
                  "humidity" {}})

(def ans1
  (let [input-data (->> "5.txt"
                        u/read-file-as-lines
                        (map str/trim)
                        (partition-by #(= "" %))
                        (filter #(not (= "" (first %)))))
        seeds (seeds-to-plant (first (first input-data)))
        mapping-defs (rest input-data)
        transform-map
        (reduce handle-mapping initial-map mapping-defs)]
    (find-lowest-location seeds transform-map)))

(def ans2
  (let [input-data (->> "5.txt"
                        u/read-file-as-lines
                        (map str/trim)
                        (partition-by #(= "" %))
                        (filter #(not (= "" (first %)))))
        seed-spans (partition 2
                              (seeds-to-plant (first (first input-data))))
        seeds (sort (flatten (map expand-span seed-spans)))
        mapping-defs (rest input-data)
        transform-map
        (reduce handle-mapping initial-map mapping-defs)]
    (find-lowest-location seeds transform-map)))

(let [input-data (->> "5b.txt"
                      u/read-file-as-lines
                      (map str/trim)
                      (partition-by #(= "" %))
                      (filter #(not (= "" (first %)))))
      seed-spans (partition 2 (seeds-to-plant (first (first input-data))))
      seeds (sort (flatten (map expand-span seed-spans)))
      mapping-defs (rest input-data)
      transform-map
      (reduce handle-mapping initial-map mapping-defs)]


  ;; (transform-id transform-map "light" "77")
  (match-sparse-mapping? 77 (first (get transform-map "light")))
)
