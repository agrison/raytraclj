(ns me.grison.raytraclj.core
  (:require [me.grison.raytraclj.image :as img]
            [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]
            [me.grison.raytraclj.hitable :as hitable]
            [me.grison.raytraclj.camera :as camera]
            [me.grison.raytraclj.material :as material]
            [flames.core :as flames])
  (:gen-class)
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File)))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn raytrace [nx ny pixels path]
  (let [header (ppm-header nx ny)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-jpg ppm path)))

(defn raytrace-direct [nx ny pixels path]
  (let [img (BufferedImage. nx ny BufferedImage/TYPE_INT_RGB)]
    (println "Writing pixels...")
    (doseq [px pixels]
      (.setRGB img (:i px) (:j px) (:c px)))
    (println "Rotating... because I'm lazy")
    (let [rotated (BufferedImage. nx ny BufferedImage/TYPE_INT_RGB)
          graphic (.createGraphics rotated)]
      (do
        (.rotate graphic (Math/toRadians 180) (/ nx 2) (/ ny 2))
        (.drawImage graphic img 0 0 nx (- ny) nil)
        ;(.drawImage graphic img nil 0 0)
        (.dispose graphic)
        (println "Saving to: " path)
        (ImageIO/write rotated "png" (File. path))))))

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

(defn color [r world depth]
  (if-let [rec (hitable/hits world r 0.0001 Float/MAX_VALUE)]
    (let [res (material/scatter (:material rec) r rec)]
      (if (and (< depth 50) (:ok res))
        (vec/* (:attenuation res) (color (:scattered res) world (inc depth)))
        [0 0 0]))
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
        (swap! col vec/+ (color r world 0))))
    (vec// @col (float ns))))

(defn make-world []
  (let [world (atom [(hitable/->Sphere [0 -1000 0] 1000 (material/->Lambertian [0.5 0.5 0.5]))])
        drand #(* (rand) (rand))]
    (doseq [a (range -11 11)
            b (range -11 11)
            :let [choose-mat (rand)
                  center [(+ a (* 0.9 (rand))) 0.2 (+ b (* 0.9 (rand)))]]]
      (if (> (vec/length (vec/- center [4 0.2 0])) 0.9)
        (cond
          (< choose-mat 0.8)                                ; diffuse
          (swap! world conj (hitable/->Sphere center 0.2 (material/->Lambertian [(drand) (drand) (drand)])))

          (< choose-mat 0.95)                               ;metal
          (swap! world conj (hitable/->Sphere center 0.2 (material/->Metal [(* 0.5 (inc (rand)))
                                                                            (* 0.5 (inc (rand)))
                                                                            (* 0.5 (rand))]
                                                                           (rand))))
          :else                                             ;glass
          (swap! world conj (hitable/->Sphere center 0.2 (material/->Dielectric 1.5)))
          )))
    (swap! world conj (hitable/->Sphere [0 1 0] 1.0 (material/->Dielectric 1.5)))
    (swap! world conj (hitable/->Sphere [-4 1 0] 1.0 (material/->Lambertian [0.4 0.2 0.1])))
    (swap! world conj (hitable/->Sphere [4 1 0] 1.0 (material/->Metal [0.7 0.6 0.5] 0.0)))
    @world))

(comment (make-world))

(defn single-px [world cam nx ny ns i j]
  (let [col (evolve-col world cam nx ny ns i j)
        corrected-col (map #(Math/sqrt %) col)
        ir (int (* 255.99 (vec/x corrected-col)))
        ig (int (* 255.99 (vec/y corrected-col)))
        ib (int (* 255.99 (vec/z corrected-col)))]
    {:i i :j j :c (bit-or (bit-shift-left ir 16)
                          (bit-shift-left ig 8)
                          ib)}))

(defn final-scene []
  (me.grison.raytraclj.perf/init)
  (let [t1 (System/currentTimeMillis)
        nx (* 120 6) ny (* 80 6) ns 10
        look-from [13 2 3]
        look-at [0 0 0]
        dist-to-focus 10
        aperture 0.1
        cam (camera/make look-from look-at [0 1 0] 20 (/ (float nx) (float ny)) aperture dist-to-focus)
        world (make-world)
        all-pixels (into [] (for [j (range (dec ny) -1 -1)
                                  i (range 0 nx)]
                              {:i i :j j}))]
    (println "Starting raytracing for " (count all-pixels) " pixels...")
    (raytrace-direct nx ny
                     (pmap #(single-px world cam nx ny ns (:i %) (:j %)) all-pixels)
                     "/mnt/c/temp/final-ir.png")
    (let [t2 (System/currentTimeMillis)
          total (/ (- t2 t1) 1000)]
      (println "-> rays/sec: " (me.grison.raytraclj.perf/rays-per-sec total)))))


(defn -main [& args]
  (time (final-scene)))


