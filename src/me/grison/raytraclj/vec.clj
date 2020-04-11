(ns me.grison.raytraclj.vec
  (:require [clojure.core :as clj]))

(defn string
  ([v] (string v false))
  ([[a b c] line-feed?]
   (str a " " b " " c " " (when line-feed? "\n"))))

(defn read [^String s]
  (clojure.string/split s #"\s+"))

(defn mute [op
            [^float x1 ^float y1 ^float z1]
            [^float x2 ^float y2 ^float z2]]
  [(op x1 x2) (op y1 y2) (op z1 z2)])

(defn + [v1 v2]
  (mute clj/+ v1 v2))

(defn - [v1 v2]
  (mute clj/- v1 v2))

(defn * [v1 v2]
  (if (number? v2)
    (map #(clj/* v2 %) v1)
    (mute clj/* v1 v2)))

(defn / [v1 v2]
  (mute clj// v1 v2))

(defn • [v1 v2]
  (reduce clj/+ (* v1 v2)))

(defn squared-length [[x y z]]
  (clj/+ (clj/* x x) (clj/* y y) (clj/* z z)))

(defn length [v]
  (Math/sqrt (squared-length v)))

(defn unit-vector [v]
  (let [l (length v)]
    (map #(clj// % l) v)))

(defn x [v]
  (first v))

(defn y [v]
  (second v))

(defn z [v]
  (last v))