(require '[clojure.string :as str])
(require '[clojure.zip :as z])
(require '[util :as u])

(def mul-re #"(mul\((\d+),(\d+)\))")

(defn exec-mul [[instruction _ a b]]
  (*
   (u/to-int a)
   (u/to-int b)))

(defn exec-mul-instructions [text]
  (let [mul-seqs (re-seq mul-re text)]
    (reduce +
            (map exec-mul mul-seqs))))

(def example "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(= 161 (exec-mul-instructions example))

