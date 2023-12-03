(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])

(defrecord Game [i gameSets])

(defn make-game-set [game-set-def]
  (let [tuple-defs (str/split game-set-def #",")
        tuples     (map #(str/split (str/trim %) #" ") tuple-defs)]
    (apply hash-map
           (flatten
            (for [[v key] tuples]
              [(keyword key) (Integer/parseInt v)])))))

(defn read-game [line]
  (let [[id-part game-sets-part] (str/split line #":")
        game-set-defs            (str/split game-sets-part #";")
        [_ game-id]              (str/split id-part #" ")
        game-sets                (map make-game-set game-set-defs)]

    (->Game (Integer/parseInt game-id) game-sets)))

(defn fewest-cubes [game]
  (let [game-sets (:gameSets game)
        colors    (apply merge {}
                         (for [which [:red :blue :green]]
                           {which (->> game-sets
                                       (map which)
                                       (filter some?)
                                       (reduce max 0))}))]
    [(:red colors)
     (:blue colors)
     (:green colors)]))

(defn power-of-game [game]
  (reduce * 1 (fewest-cubes game)))

(let [games (->> "2.txt"
                 u/read-file-as-lines
                 (map read-game))]
  ;; 67335
  (reduce + (map power-of-game games)))
