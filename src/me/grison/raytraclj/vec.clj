(ns me.grison.raytraclj.vec
  (:require [clojure.core :as clj]))

(defn string
  ([v] (string v))
  ([[a b c] line-feed?]
  (str a " " b " " c " " (when line-feed? "\n"))))

(defn read [^String s]
  (clojure.string/split s "\s+"))