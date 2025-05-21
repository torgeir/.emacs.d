#!/usr/bin/env bb

;; SPC m '
;; cider-jack-in-clj

(ns user
  (:require [babashka.deps :as deps]))


(deps/add-deps '{:deps {lambdaisland/deep-diff2 {:mvn/version "2.7.169"}}})
(require '[lambdaisland.deep-diff2 :refer [pretty-print diff]])

(pretty-print (diff {:a 1 :b 2} {:a 1 :b (range 10)}))


(defn read-json [file-path]
  (cheshire.core/decode (->> file-path slurp) true))


(->> [1 2 2 3 3 3 4 4 4 4] frequencies println)
