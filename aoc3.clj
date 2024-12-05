(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])

(def mul-re #"(mul\((\d+),(\d+)\))")
(def inst-re #"(don't\(\))|(do\(\))|(mul\((\d+),(\d+)\))")


(defn clear-mul [state]
  (assoc state :enabled? false))

(defn enable-mul [state]
  (assoc state :enabled? true))

(defn yield-mul [a b]
  (fn [state]
    (update state :val
            #(+ %
                (if (:enabled? state)
                  (* (u/to-int a)
                     (u/to-int b))
                  0)))))

(defn yield-op [[instruction _ _ _ a b]]
  (cond
    (= instruction "don't()") clear-mul
    (= instruction "do()") enable-mul
    :else (yield-mul a b)))


(defn apply-op [state op]
  (op state))

(defn exec-instructions [text]
  (let [inst-seqs (re-seq inst-re text)]
    (:val
     (reduce apply-op
             {:enabled? true :val 0}
             (map yield-op inst-seqs)))))

(def example "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(= 48 (exec-instructions example))

