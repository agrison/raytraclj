(ns me.grison.raytraclj.core
  (:require [me.grison.raytraclj.image :as img]
            [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn raytrace [nx ny pixels path]
  (let [header (ppm-header nx ny)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-jpg ppm path)))

(defn color [r]
  (let [t (ray/hit-sphere [0.0 0.0 -1.0] 0.5 r)]
    (if (pos? t)
      (let [N (vec/unit-vector (vec/- (ray/point-at-parameter r t)
                                      [0.0 0.0 -1.0]))]
        (vec/* (map inc N) 0.5))
      (let [unit-direction (vec/unit-vector (ray/direction r))
            t (* 0.5 (inc (vec/y unit-direction)))]
        (vec/+ (vec/* [1.0 1.0 1.0] (- 1.0 t))
               (vec/* [0.5 0.7 1.0] t))))))

(defn simple-background-and-sphere-surface []
  (let [nx 800 ny 400
        lower-left-corner [-2.0 -1.0 -1.0]
        horizontal [4.0 0.0 0.0]
        vertical [0.0 2.0 0.0]
        origin [0.0 0.0 0.0]]
    (raytrace nx ny
              (for [j (range (dec ny) -1 -1)
                    i (range 0 nx)
                    :let [u (/ i nx)
                          v (/ j ny)
                          r (ray/make origin (vec/+ lower-left-corner
                                                    (vec/+ (vec/* horizontal u)
                                                           (vec/* vertical v))))
                          col (color r)
                          ir (int (* 255.99 (vec/x col)))
                          ig (int (* 255.99 (vec/y col)))
                          ib (int (* 255.99 (vec/z col)))]]
                (vec/string [ir ig ib]))
              "/mnt/c/temp/background-sphere-surface.jpg")))


; run this
(comment (simple-background-and-sphere-surface))