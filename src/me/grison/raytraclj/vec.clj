(ns me.grison.raytraclj.vec
  (:require [clojure.core :as clj]))

(defn string
  ([v] (string v false))
  ([[a b c] line-feed?]
   (str a " " b " " c " " (when line-feed? "\n"))))

(defn read [^String s]
  (clojure.string/split s #"\s+"))

(defn mute [op [x1 y1 z1] [x2 y2 z2]]
  [(op x1 x2) (op y1 y2) (op z1 z2)])

(defn + [v1 v2]
  (mute clj/+ v1 v2))

(defn * [v1 v2]
  (mute clj/* v1 v2))

(defn *1 [v n]
  (map #(clj/* n %) v))

(defn / [v1 v2]
  (mute clj// v1 v2))

(defn unit-vector [v]
  (vec (for [elem v]
         (clj// elem (count v)))))

(defn x [v]
  (first v))

(defn y [v]
  (second v))

(defn z [v]
  (last v))