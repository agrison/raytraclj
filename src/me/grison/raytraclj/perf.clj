(ns me.grison.raytraclj.perf
  (:import (java.util.concurrent.atomic AtomicInteger)))

(def ^:dynamic rays (AtomicInteger. 0))

(defn init []
  (.set rays 0))

(defn inc-rays []
  (.incrementAndGet rays))

(defn rays-per-sec [seconds]
  (int (/ (.get rays) seconds)))
