(ns me.grison.raytraclj.core
  (:require [me.grison.raytraclj.image :as img]
            [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]
            [me.grison.raytraclj.hitable :as hitable]))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn raytrace [nx ny pixels path]
  (let [header (ppm-header nx ny)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-jpg ppm path)))

(defn color [r world]
  (if-let [rec (hitable/hits world r 0.0 Float/MAX_VALUE)]
    (do
      (vec/* (map inc (:normal rec)) 0.5))
    (let [unit-direction (vec/unit-vector (ray/direction r))
          t (* 0.5 (inc (vec/y unit-direction)))]
      (vec/+ (vec/* [1.0 1.0 1.0] (- 1.0 t))
             (vec/* [0.5 0.7 1.0] t)))))

(defn simple-background-and-sphere-surface []
  (let [nx 1280 ny (/ 1280 2)
        lower-left-corner [-2.0 -1.0 -1.0]
        horizontal [4.0 0.0 0.0]
        vertical [0.0 2.0 0.0]
        origin [0.0 0.0 0.0]
        world [(hitable/->Sphere [0 0 -1] 0.5)
               (hitable/->Sphere [0 -100.5 -1] 100)]]
    (raytrace nx ny
              (for [j (range (dec ny) -1 -1)
                    i (range 0 nx)
                    :let [u (/ i nx)
                          v (/ j ny)
                          r (ray/make origin (vec/+ lower-left-corner
                                                    (vec/+ (vec/* horizontal u)
                                                           (vec/* vertical v))))
                          ;p (ray/point-at-parameter r 2.0)
                          col (color r world)
                          ;_ (println col)
                          ir (int (* 255.99 (vec/x col)))
                          ig (int (* 255.99 (vec/y col)))
                          ib (int (* 255.99 (vec/z col)))]]
                (vec/string [ir ig ib]))
              "/mnt/c/temp/background-sphere-surface-2.jpg")))


; run this
(comment (simple-background-and-sphere-surface))