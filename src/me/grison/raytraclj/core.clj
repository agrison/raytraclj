(ns me.grison.raytraclj.core
  (:require [me.grison.raytraclj.image :as img]))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn pixel-line [r g b]
  (str r " " g " " b "\n"))

(defn hello-ppm []
  (let [nx 200
        ny 100
        header (str "P3\n" nx " " ny "\n255\n")
        pixels (for [j (range (dec ny) -1 -1)
                     i (range 0 nx)
                     :let [r (int (* 255.99 (/ i nx)))
                           g (int (* 255.99 (/ j ny)))
                           b (int (* 255.99 0.2))]]
                 (pixel-line r g b))
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-jpg ppm "/mnt/c/temp/first")))

(comment (hello-ppm))