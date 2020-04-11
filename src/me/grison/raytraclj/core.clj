(ns me.grison.raytraclj.core
  (:require [me.grison.raytraclj.image :as img]
            [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]
            [me.grison.raytraclj.hitable :as hitable]
            [me.grison.raytraclj.camera :as camera])
  (:import (java.util Random)))

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

(defn drand-48 []
  (.nextFloat (Random.)))

(defn evolve-col [world cam nx ny ns i j]
  (let [col (atom [0 0 0])]
    (doseq [s (range ns)]
      (let [u (/ (+ i (drand-48)) (float nx))
            v (/ (+ j (drand-48)) (float ny))
            r (camera/get-ray cam u v)]
        (swap! col vec/+ (color r world))))
    (vec// @col (float ns))))

(defn simple-background-and-sphere-surface-antialias []
  (let [nx 400 ny (/ nx 2) ns 50
        cam (camera/make [-2.0 -1.0 -1.0]
                         [4.0 0.0 0.0]
                         [0.0 2.0 0.0]
                         [0.0 0.0 0.0])
        world [(hitable/->Sphere [0 0 -1] 0.5)
               (hitable/->Sphere [0 -100.5 -1] 100)]]
    (raytrace nx ny
              (for [j (range (dec ny) -1 -1)
                    i (range 0 nx)
                    :let [col (evolve-col world cam nx ny ns i j)
                          ir (int (* 255.99 (vec/x col)))
                          ig (int (* 255.99 (vec/y col)))
                          ib (int (* 255.99 (vec/z col)))]]
                (vec/string [ir ig ib]))
              "/mnt/c/temp/background-sphere-surface-antialias.jpg")))


; run this
(comment (simple-background-and-sphere-surface-antialias))

(time (simple-background-and-sphere-surface-antialias))