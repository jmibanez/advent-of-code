;; utility functions for

(ns util
  (:require [clojure.string :as str]))


(defn read-file-as-lines [file-name]
  (-> file-name
      slurp
      str/trim
      (str/split #"\n")))

(defn to-int [s]
  (Integer/parseInt s))
