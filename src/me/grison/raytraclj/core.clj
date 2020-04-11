(ns me.grison.raytraclj.core
  (:require [me.grison.raytraclj.image :as img]
            [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]
            [me.grison.raytraclj.hitable :as hitable]
            [me.grison.raytraclj.camera :as camera])
  (:gen-class))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn raytrace [nx ny pixels path]
  (let [header (ppm-header nx ny)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-jpg ppm path)))

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

(defn color [r world]
  (if-let [rec (hitable/hits world r 0.0001 Float/MAX_VALUE)]
    (let [target (vec/+ (vec/+ (:p rec) (:normal rec)) (random-in-unit-sphere))]
      (vec/* (color (ray/make (:p rec) (vec/- target (:p rec))) world) 0.5))
    (let [unit-direction (vec/unit-vector (ray/direction r))
          t (* 0.5 (inc (vec/y unit-direction)))]
      (vec/+ (vec/* [1.0 1.0 1.0] (- 1.0 t))
             (vec/* [0.5 0.7 1.0] t)))))

(defn evolve-col [world cam nx ny ns i j]
  (let [col (atom [0 0 0])]
    (doseq [_ (range ns)]
      (let [u (/ (+ i (rand)) (float nx))
            v (/ (+ j (rand)) (float ny))
            r (camera/get-ray cam u v)]
        (swap! col vec/+ (color r world))))
    (vec// @col (float ns))))

(defn simple-background-and-sphere-surface-antialias []
  (let [nx 800 ny (/ nx 2) ns 30
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
                          corrected-col (map #(Math/sqrt %) col)
                          ir (int (* 255.99 (vec/x corrected-col)))
                          ig (int (* 255.99 (vec/y corrected-col)))
                          ib (int (* 255.99 (vec/z corrected-col)))]]
                (vec/string [ir ig ib]))
              "/mnt/c/temp/background-sphere-surface-antialias-diffuse-2.jpg")))

(defn -main [& args]
  (time (simple-background-and-sphere-surface-antialias)))



; run this
;(comment (time (simple-background-and-sphere-surface-antialias)))
