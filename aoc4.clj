(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :as m])
(require '[util :as u])

(defn parse-line [line]
  (let [[meta details] (str/split line #":")
        [winning mine] (->> (str/split details #"\|")
                            (map str/trim)
                            (map #(str/split % #"( )+")))
        [_ card] (str/split meta #"( )+")]
    {:card (Integer/parseInt card)
     :winning (into (sorted-set) (map #(Integer/parseInt %) winning))
     :mine  (into (sorted-set) (map #(Integer/parseInt %) mine))}))


(defn get-matches [card]
  (let [{winning :winning
         mine    :mine} card]
    (set/intersection winning mine)))

(defn get-match-counts-for-card [card]
  (count (get-matches card)))


(defn calculate-points [card]
  (let [my-winnings (get-match-counts-for-card card)]
    (if (> my-winnings 0)
      (m/pow 2 (- my-winnings 1))
      0)))

(defn inc-or-default [v]
  (if (some? v)
    (inc v)
    1))

(defn calculate-scoreboard [limit scoreboard card]
  (let [card-id (:card card)
        card-counts (get-match-counts-for-card card)
        card-id-bounds (min limit (+ card-id card-counts))
        copies-of-self (get scoreboard card-id 0)]

    (if (zero? card-counts)
      scoreboard
      (loop [counter copies-of-self
             scoreboard scoreboard]
        (if (< counter 0)
          scoreboard
          (recur
           (dec counter)
           (loop [next-card-id (inc card-id)
                  new-scoreboard scoreboard]
             (if (<= next-card-id card-id-bounds)
               (recur (inc next-card-id)
                      (update new-scoreboard next-card-id inc-or-default))
               new-scoreboard))))))))


(def ans1
  (->> (->> "4.txt"
            u/read-file-as-lines
            (map parse-line))
       (map calculate-points)
       (reduce +)))

(def ans2
  (let [cards (->> "4.txt"
                   u/read-file-as-lines
                   (map parse-line))
        limit (count cards)]

    (+ (reduce +
               (vals (reduce (partial calculate-scoreboard limit)
                             {} cards)))
       limit)))
