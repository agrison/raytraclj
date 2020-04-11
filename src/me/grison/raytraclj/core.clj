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
  (if (ray/hit-sphere [0 0 -1] 0.5 r)
    [1 0 0]
    (let [unit-direction (vec/unit-vector (ray/direction r))
          t (* 0.5 (+ (vec/y unit-direction) 1.0))]
      (vec/+ (vec/*1 [1.0 1.0 1.0] (- 1.0 t))
             (vec/*1 [0.5 0.7 1.0] t)))))

(defn simple-background-and-sphere []
  (let [nx 200 ny 100
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
                                                    (vec/+ (vec/*1 horizontal u)
                                                           (vec/*1 vertical v))))
                          col (color r)
                          ir (int (* 255.99 (vec/x col)))
                          ig (int (* 255.99 (vec/y col)))
                          ib (int (* 255.99 (vec/z col)))]]
                (vec/string [ir ig ib]))
              "/mnt/c/temp/background-sphere.jpg")))


; run this
(comment (simple-background-and-sphere))