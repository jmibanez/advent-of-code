(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])

(defn read-input [input]
  (-> input
      slurp
      str/trim
      (str/split #"\n\n")))

(defn ->ordering-rule [[p1 p2]]
  (fn [state-vec current-page]
    (cond
      (and (= p2 current-page) (some #{p1} state-vec)) true
      (and (= p1 current-page) (not (some #{p2} state-vec))) true
      (and (= p1 current-page) (some #{p2} state-vec)) false
      :else true)))

(defn parse-ordering-rule [rule-str]
  (-> rule-str
      (str/split #"\|")
      ->ordering-rule))

(defn read-rules [input]
  (let* [raw-tuples (read-input input)
         page-ordering-rules-lines (str/split (nth raw-tuples 0) #"\n")
         update-rules-lines (str/split (nth raw-tuples 1) #"\n")]
    {:ordering-rules (map parse-ordering-rule page-ordering-rules-lines)
     :update-rules (map #(str/split % #",") update-rules-lines)}))

(defn ordering-reduction [ordering-rules]
  (fn [state-vec current-page]
    (if (every? #(% state-vec current-page) ordering-rules)
      (conj state-vec current-page)
      [])))

(defn is-update-good? [ordering-rules update-rule]
  (= update-rule
     (reduce (ordering-reduction ordering-rules) [] update-rule)))

(defn good-rules [{ordering-rules :ordering-rules
                   update-rules :update-rules}]
  (filter #(is-update-good? ordering-rules %) update-rules))

(defn middle-value [update-rule]
  (u/to-int
   (nth update-rule (quot (count update-rule) 2))))

(def example-rules (read-rules "aoc5-input.txt"))
(def ans
  (reduce + (map middle-value (good-rules example-rules))))

