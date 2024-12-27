(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])

(defn read-input [input]
  (-> input
      slurp
      str/trim
      (str/split #"\n\n")))

(defn ->ordering-rule [[s-p1 s-p2]]
  (let [p1 (u/to-int s-p1)
        p2 (u/to-int s-p2)]
    (fn [state-vec current-page]
      (cond
        (and (= p2 current-page) (some #{p1} state-vec)) true
        (and (= p1 current-page) (not (some #{p2} state-vec))) true
        (and (= p1 current-page) (some #{p2} state-vec)) false
        :else true))))

(defn reorder [state-vec p2 p1]
  (let [[before after] (split-at (- (.indexOf state-vec p2) 1) state-vec)
        candidate-vec (vec (concat before [p1] after))]
    [(pop candidate-vec) (last candidate-vec)]))

(defn ->edit-rule [[s-p1 s-p2]]
  (let [p1 (u/to-int s-p1)
        p2 (u/to-int s-p2)]
    (fn _edit
      ([[state-vec current-page]]
       (cond
         (and (= p2 current-page) (some #{p1} state-vec))
         [state-vec current-page]

         (and (= p1 current-page) (not (some #{p2} state-vec)))
         [state-vec current-page]

         (and (= p1 current-page) (some #{p2} state-vec))
         (reorder state-vec p2 p1)

         :else
         [state-vec current-page]))
      ([state-vec current-edit]
       (_page [state-vec current-page])))))

(defn ->ordering-rule-map [rule]
  {:check (->ordering-rule rule)
   :reorder (->edit-rule rule)})

(defn parse-ordering-rule [rule-str]
  (-> rule-str
      (str/split #"\|")
      ->ordering-rule-map))

(defn s-vec->int-vec [s-vec]
  (map u/to-int s-vec))

(defn read-rules [input]
  (let* [raw-tuples (read-input input)
         page-ordering-rules-lines (str/split (nth raw-tuples 0) #"\n")
         update-rules-lines (str/split (nth raw-tuples 1) #"\n")]
    {:ordering-rules (map parse-ordering-rule page-ordering-rules-lines)
     :update-rules (map s-vec->int-vec
                        (map #(str/split % #",") update-rules-lines))}))

(defn ordering-reduction [ordering-rules]
  (fn [state-vec current-page]
    (if (every? #((:check %) state-vec current-page) ordering-rules)
      (conj state-vec current-page)
      [])))

(defn reordering-reduction [ordering-rules]
  (fn [state-vec current-page]
    (let [rule-comp (apply comp (map :reorder ordering-rules))
          [new-state-vec new-page] (apply rule-comp [state-vec current-page])]
      (conj new-state-vec new-page))))

(defn apply-edits [ordering-rules state-vec]
  (reduce (reordering-reduction ordering-rules) [] state-vec))

(defn ->editor [{ordering-rules :ordering-rules}]
  (fn [state-vec]
    (loop [current-state state-vec]
      (if (not (is-update-good? ordering-rules current-state))
        (recur (apply-edits ordering-rules current-state))
        current-state))))

(defn is-update-good? [ordering-rules update-rule]
  (= update-rule
     (reduce (ordering-reduction ordering-rules) [] update-rule)))

(defn good-rules [{ordering-rules :ordering-rules
                   update-rules :update-rules}]
  (filter #(is-update-good? ordering-rules %) update-rules))

(defn bad-rules [{ordering-rules :ordering-rules
                  update-rules :update-rules}]
  (filter #(not (is-update-good? ordering-rules %)) update-rules))


(defn middle-value [update-rule]
  (nth update-rule (quot (count update-rule) 2)))

(def example-rules (read-rules "aoc5-input.txt"))

(def ans-p1
  (reduce + (map middle-value (good-rules example-rules))))

(def ans-p2
  (reduce + (map middle-value
                 (map (->editor example-rules)
                      (bad-rules example-rules)))))

