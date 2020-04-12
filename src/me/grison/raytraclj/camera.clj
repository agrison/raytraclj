(ns me.grison.raytraclj.camera
  (:require [me.grison.raytraclj.ray :as ray]
            [me.grison.raytraclj.vec :as vec]))

(defn random-in-unit-disk []
  (loop [p (vec/- (vec/* [(rand) (rand) 0] 2) [1 1 0])]
    (if (< (vec/• p p) 1)
      p
      (recur (vec/- (vec/* [(rand) (rand) 0] 2) [1 1 0])))))

(defn make [look-from look-at vup vfov aspect aperture focus-dist]
  (let [lens-radius (/ aperture 2)
        theta (/ (* vfov Math/PI) 180)
        half-height (Math/tan (/ theta 2))
        half-width (* aspect half-height)
        origin look-from
        w (vec/unit-vector (vec/- look-from look-at))
        u (vec/unit-vector (vec/⨯ vup w))
        v (vec/⨯ w u)
        lower-left-corner (vec/- (vec/- (vec/- origin
                                               (vec/* u (* half-width focus-dist)))
                                        (vec/* v (* half-height focus-dist)))
                                 (vec/* w focus-dist))
        horizontal (vec/* u (* 2 half-width focus-dist))
        vertical (vec/* v (* 2 half-height focus-dist))]
    {:lower-left-corner lower-left-corner
     :horizontal        horizontal
     :vertical          vertical
     :origin            origin
     :lens-radius       lens-radius
     :u                 u
     :v                 v
     :w                 w}))

(defn get-ray [{:keys [lower-left-corner horizontal vertical
                       origin lens-radius u v]} s t]
  (let [rd (vec/* (random-in-unit-disk) lens-radius)
        offset (vec/+ (vec/* u (vec/x rd)) (vec/* v (vec/y rd)))]
    (ray/make (vec/+ origin offset)
              (vec/+ lower-left-corner
                     (vec/- (vec/- (vec/+ (vec/* horizontal s) (vec/* vertical t))
                                   origin)
                            offset)))))