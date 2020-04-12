(ns me.grison.raytraclj.core
  (:require [me.grison.raytraclj.image :as img]
            [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]
            [me.grison.raytraclj.hitable :as hitable]
            [me.grison.raytraclj.camera :as camera]
            [me.grison.raytraclj.material :as material]
            [flames.core :as flames])
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

(def BLAH (atom nil))

(defn color [r world depth]
  (if-let [rec (hitable/hits world r 0.0001 Float/MAX_VALUE)]
    (let [res (material/scatter (:material rec) r rec)]
      (if (and (< depth 50) (:ok res))
        ;(println "res: " res " world: " world " depth: " depth )
        ;(reset! BLAH res)
        (vec/* (:attenuation res) (color (:scattered res) world (inc depth)))
        [0 0 0]))
    (let [;_ (println "in color - r : " r)
          unit-direction (vec/unit-vector (ray/direction r))
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

(defn simple-background-and-sphere-dielectric []
  (me.grison.raytraclj.perf/init)
  (let [t1 (System/currentTimeMillis)
        nx 500 ny (/ nx 2) ns 30
        cam (camera/make [-2 2 1] [0 0 -1] [0 1 0] 30 (/ (float nx) (float ny)))
        R (Math/cos (/ Math/PI 4))
        world [
               ;(hitable/->Sphere [(- R) 0 -1] R (material/->Lambertian [0 0 1]))
               ;(hitable/->Sphere [R 0 -1] R (material/->Lambertian [1 0 0]))
               (hitable/->Sphere [0 0 -1] 0.5 (material/->Lambertian [0.1 0.2 0.5]))
               (hitable/->Sphere [0 -100.5 -1] 100 (material/->Lambertian [0.8 0.8 0.0]))
               (hitable/->Sphere [1 0 -1] 0.5 (material/->Metal [0.8 0.6 0.2] 1.0))
               ;(hitable/->Sphere [-1 0 -1] 0.5 (material/->Metal [0.8 0.8 0.8] 0.3))
               (hitable/->Sphere [-1 0 -1] 0.5 (material/->Dielectric 1.5))
               (hitable/->Sphere [-1 0 -1] -0.45 (material/->Dielectric 1.5))
               ]]
    (raytrace nx ny
              (for [j (range (dec ny) -1 -1)
                    i (range 0 nx)
                    :let [col (evolve-col world cam nx ny ns i j)
                          corrected-col (map #(Math/sqrt %) col)
                          ir (int (* 255.99 (vec/x corrected-col)))
                          ig (int (* 255.99 (vec/y corrected-col)))
                          ib (int (* 255.99 (vec/z corrected-col)))]]
                (vec/string [ir ig ib]))
              "/mnt/c/temp/background-sphere-cam1.jpg")
    (let [t2 (System/currentTimeMillis)
          total (/ (- t2 t1) 1000)]
      (println "-> rays/sec: " (me.grison.raytraclj.perf/rays-per-sec total)))))

(comment (def flames (flames/start! {:port 54321, :host "localhost"})))

(comment (flames/stop! flames))

(defn -main [& args]
  (time (simple-background-and-sphere-dielectric)))



; run this
;(comment (time (simple-background-and-sphere-surface-antialias)))
