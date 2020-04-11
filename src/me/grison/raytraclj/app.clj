(ns me.grison.raytraclj.app
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:import javax.imageio.ImageIO))

(defn file->bytes [file]
  (with-open [xin (io/input-stream file)
              xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

(defn path->buffered-image [path]
  (with-open [in (io/input-stream (io/file path))]
    (ImageIO/read in)))

(defn buffered-image->path [img path]
  (ImageIO/write img "JPEG" (io/file path)))

(defn -main [& args]
  (buffered-image->path (path->buffered-image "C:/temp/foo.ppm") "C:/temp/foo.jpg"))