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

(defn schlick [cosine ref-idx]
  (let [r0 (/ (- 1 ref-idx)
              (+ 1 ref-idx))
        r0 (* r0 r0)]
    (+ r0 (* (- 1 r0)
             (Math/pow (- 1 cosine) 5)))))

(defrecord Dielectric [ref-idx]
  Material
  (scatter [this r-in rec]
    (let [reflected (vec/reflect (:direction r-in) (:normal rec))
          attenuation [1.0 1.0 1.0]
          dot-dir-normal (vec/• (:direction r-in) (:normal rec))
          ni-over-nt (if (pos? dot-dir-normal)
                       (:ref-idx this)
                       (/ 1.0 (:ref-idx this)))
          outward-normal (if (pos? dot-dir-normal)
                           (vec/- [0 0 0] (:normal rec))
                           (:normal rec))
          cosine (if (pos? dot-dir-normal)
                   (/ (* (:ref-idx this) (vec/• (:direction r-in) (:normal rec)))
                      (vec/length (:direction r-in)))
                   (/ (- (vec/• (:direction r-in) (:normal rec)))
                      (vec/length (:direction r-in))))
          refracted (vec/refract (:direction r-in) outward-normal ni-over-nt)
          reflect-prob (if (not (nil? refracted))
                         (schlick cosine (:ref-idx this))
                         1.0)]
      (if (< (rand) reflect-prob)
        {:ok true :attenuation attenuation :scattered (ray/make (:p rec) reflected)}
        {:ok true :attenuation attenuation :scattered (ray/make (:p rec) refracted)}))))