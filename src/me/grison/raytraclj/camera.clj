(ns me.grison.raytraclj.camera
  (:require [me.grison.raytraclj.ray :as ray]
            [me.grison.raytraclj.vec :as vec]))

(defn make [look-from look-at vup vfov aspect]
  (let [theta (/ (* vfov Math/PI) 180)
        half-height (Math/tan (/ theta 2))
        half-width (* aspect half-height)
        origin look-from
        w (vec/unit-vector (vec/- look-from look-at))
        u (vec/unit-vector (vec/⨯ vup w))
        v (vec/⨯ w u)
        lower-left-corner (vec/- (vec/- (vec/- origin
                                               (vec/* u half-width))
                                        (vec/* v half-height))
                                 w)
        horizontal (vec/* u (* 2 half-width))
        vertical (vec/* v (* 2 half-height))]
    {:lower-left-corner lower-left-corner
     :horizontal        horizontal
     :vertical          vertical
     :origin            origin}))

(defn get-ray [{:keys [lower-left-corner
                       horizontal
                       vertical
                       origin]} u v]
  (ray/make origin
            (vec/- (vec/+ lower-left-corner
                          (vec/+ (vec/* horizontal u)
                                 (vec/* vertical v)))
                   origin)))
