(ns index
  (:require [clojure.test :as t]))

(defn fly-tests []
  (inc "sss")
  (+ 2 1))

(defn count-loop [n]
  (loop [n n]
    (when (> n 0)
      (println "now" n)
      (recur (dec n)))))

(count-loop 10)
