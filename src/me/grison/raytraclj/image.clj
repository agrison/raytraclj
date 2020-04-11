(ns me.grison.raytraclj.image
  (:require [clojure.java.io :as io])
  (:import javax.imageio.ImageIO
           (java.awt.image BufferedImage)
           (java.io ByteArrayInputStream)))

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

(defn ppm->image [^String ppm]
  (with-open [in (java.io.ByteArrayInputStream. (.getBytes ppm))]
    (ImageIO/read in)))

(defn save-jpg [^String ppm ^String path]
  ;(save-ppm ppm (str path ".ppm"))
  ;(Thread/sleep 500)
  (store-jpeg (ppm->image ppm) path))