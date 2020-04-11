(ns me.grison.raytraclj.ray
  (:require [me.grison.raytraclj.vec :as vec]))

(defn make
  [origin direction]
  {:origin origin :direction direction})

(defn origin [ray]
  (:origin ray))

(defn direction [ray]
  (:direction ray))
