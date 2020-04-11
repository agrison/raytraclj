(ns me.grison.raytraclj.ray
  (:require [me.grison.raytraclj.vec :as vec]))

(defn make
  [origin direction]
  {:origin origin :direction direction})

(defn origin [ray]
  (:origin ray))

(defn direction [ray]
  (:direction ray))

(defn point-at-parameter
  [ray t]
  (vec/+ (origin ray)
         (vec/* t (direction ray))))

(defn hit-sphere
  [center radius {:keys [origin direction]}]
  (let [oc (vec/- origin center)
        a (vec/• direction direction)
        b (* 2.0 (vec/• oc direction))
        c (- (vec/• oc oc) (* radius radius))
        discriminant (- (* b b) (* 4 a c))]
    (> discriminant 0)))