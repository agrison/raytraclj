(ns me.grison.raytraclj.vec
  (:require [clojure.core :as clj]))

(defn mute [op
            [^float x1 ^float y1 ^float z1]
            [^float x2 ^float y2 ^float z2]]
  [(op x1 x2) (op y1 y2) (op z1 z2)])

(defn + [v1 v2]
  (mute clj/+ v1 v2))

(defn - [v1 v2]
  (if (number? v2)
    (map #(clj/- v2 %) v1)
    (mute clj/- v1 v2)))

(defn * [v1 v2]
  (if (number? v2)
    (map #(clj/* v2 %) v1)
    (mute clj/* v1 v2)))

(defn / [v1 v2]
  (if (number? v2)
    (if (zero? v2)
      1
      (* v1 (clj// 1 v2)))
    (mute clj// v1 v2)))

(defn • [v1 v2]
  (reduce clj/+ (* v1 v2)))

(defn ⨯ [[x1 y1 z1] [x2 y2 z3]]
  [(clj/- (clj/* y1 z3) (clj/* z1 y2))
   (clj/- (clj/* z1 x2) (clj/* x1 z3))
   (clj/- (clj/* x1 y2) (clj/* y1 x2))])

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

(defn reflect [v n]
  (let [m (* n (clj/* (• v n) 2))]
    (- v m)))

(defn refract [v n ni-over-nt]
  (let [uv (unit-vector v)
        dt (• uv n)
        discriminant (clj/- 1.0
                        (clj/* ni-over-nt ni-over-nt (clj/- 1 (clj/* dt dt))))]
    (when (pos? discriminant)
      (let [uv-ndt (- uv (* n dt))
            n-discrim (* n (Math/sqrt discriminant))]
        (- (* uv-ndt ni-over-nt) n-discrim)))))