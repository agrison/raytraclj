(ns me.grison.raytraclj.image
  (:require [clojure.java.io :as io])
  (:import javax.imageio.ImageIO
           (java.awt.image BufferedImage)))

(defn load-ppm [^String path]
  (println "Loading PPM: " path)
  (with-open [in (io/input-stream (io/file path))]
    (ImageIO/read in)))

(defn save-ppm [^String ppm ^String path]
  (println "Saving PPM: " path)
  (spit path ppm))

(defn store-jpeg [^BufferedImage img ^String path]
  (println "Writing JPEG: " path)
  (ImageIO/write img "JPEG" (io/file path)))

; not very efficient for now
(defn save-jpg [^String ppm ^String path]
  (save-ppm ppm (str path ".ppm"))
  (Thread/sleep 500)
  (store-jpeg (load-ppm (str path ".ppm")) (str path ".jpg")))