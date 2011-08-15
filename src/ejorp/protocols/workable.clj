(ns ejorp.protocols.workable
  (:require [ejorp.protocols.load-traj :as load-traj]))

(defprotocol Workable
  (start-date [workable])
  (end-date [workable]))

(defn duration
  "Computes the duration of a Workable"
  [workable]
  (let [s-time (.getMillis (start-date workable))
        e-time (.getMillis (end-date workable))
        delta (Math/abs (- e-time s-time))
        num-days (/ delta 1000.0 60.0 60.0 24.0)]
    num-days))

(defn fraction-of
  "Returns the fraction that a date is in a workable"
  [workable date]
  (load-traj/fraction-of (start-date workable) (end-date workable) date))
      
(defn clamp-date
  "This clamps a date to a workable's date range."
  [workable date]
  (load-traj/clamp-date (start-date workable) (end-date workable) date))
