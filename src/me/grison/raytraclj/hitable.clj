(ns me.grison.raytraclj.hitable
  (:require [me.grison.raytraclj.vec :as vec]
            [me.grison.raytraclj.ray :as ray]))

(defprotocol Hitable
  (hit [this r t-min t-max]))

(defn hit-record [r t center radius]
  (let [p (ray/point-at-parameter r t)]
    {:t t :p p :normal (vec// (vec/- p center) radius)}))

(defrecord Sphere [center radius]
  Hitable
  (hit [this r t-min t-max]
    (let [oc (vec/- (ray/origin r) (:center this))
          a (vec/squared-length (ray/direction r))
          half-b (vec/• oc (ray/direction r))
          c (- (vec/squared-length oc) (* (:radius this) (:radius this)))
          discriminant (- (* half-b half-b) (* a c))]
      (when (pos? discriminant)
        (let [root (Math/sqrt discriminant)
              temp (/ (- (- half-b) root) a)]
          (if (and (< temp t-max) (> temp t-min))
            (hit-record r temp (:center this) (:radius this))
            (let [temp (/ (+ (- half-b) root) a)]
              (when (and (< temp t-max) (> temp t-min))
                (hit-record r temp (:center this) (:radius this))))))))))

(defn hits [world r t-min t-max]
  (let [closest-so-far (atom t-max)
        record (atom nil)]
    (doseq [i (range 0 (count world))]
      (do
        (if-let [rec (hit (get world i) r t-min @closest-so-far)]
          (do
            (reset! closest-so-far (:t rec))
            (reset! record rec)))))
    @record))
