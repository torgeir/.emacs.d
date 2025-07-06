(ns clj.core
  (:require [java-time.api :as t]))

;; clj -X:deps find-versions :lib clojure.java-time/clojure.java-time

(str (t/instant))

(defn collatz [start]
  (loop [n start acc nil]
    (if (<= n 1)
      acc
      (let [next (if (odd? n)
                   (+ 1 (* n 3))
                   (/ n 2))]
        (recur next (conj acc next))))))

(->> (repeatedly #(+ 5 (rand-int 1000000000)))
    (take 1000)
    (map collatz)
    (map #(take 3 %))
    (every? #(= '(1 2 4) %))) ; => true
