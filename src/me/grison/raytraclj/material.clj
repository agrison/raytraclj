(ns me.grison.raytraclj.material
  (:require [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]))

(defn random-in-unit-sphere []
  (let [rand-vec #(vec/-
                    (vec/* [(rand) (rand) (rand)] 2.0)
                    [1.0 1.0 1.0])
        p (atom nil)]
    (do
      (reset! p (rand-vec))
      (while (>= (vec/squared-length @p) 1.0)
        (reset! p (rand-vec))))
    @p))

(defprotocol Material
  (scatter [this r-in rec]))

(defrecord Lambertian [a]
  Material
  (scatter [this r-in rec]
    (let [target (vec/+ (vec/+ (:p rec) (:normal rec)) (random-in-unit-sphere))
          scattered (ray/make (:p rec) (vec/- target (:p rec)))]
      {:ok true :attenuation (:a this) :scattered scattered})))

(defrecord Metal [a f]
  Material
  (scatter [this r-in rec]
    (let [fuzz (if (< f 1) f 1)
          ;_ (println "Reflect? -> direction: " (:direction r-in) " normal: " (:normal rec))
          reflected (vec/reflect (vec/unit-vector (:direction r-in)) (:normal rec))
          scattered (ray/make (:p rec) (vec/+ reflected (vec/* (random-in-unit-sphere) fuzz)))
          final (vec/• (:direction scattered) (:normal rec))]
      {:ok (pos? final) :attenuation (:a this) :scattered scattered})))