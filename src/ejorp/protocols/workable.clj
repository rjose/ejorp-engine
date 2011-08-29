(ns ejorp.protocols.workable
  (:require [ejorp.protocols.traj :as traj]))

;(workable/set-named-traj-fn sample-workable :planned-by-role traj-map1)
(defprotocol Workable
  (date-map-ref [w])
  (named-traj-map-ref [w])
  (traj-map-ref [w]))

(defn set-dates
  "This sets the dates for a workable for a given key `k`"
  [w k dates]
  (let [my-ref (date-map-ref w)
        new-map (assoc @my-ref k dates)]
    (ref-set my-ref new-map)))

(defn get-dates
  "This gets the dates for a given key `k`"
  [w k]
  (k @(date-map-ref w)))

(defn duration
  "Computes the duration of a w"
  [w & k]
  (let [my-k (if k (first k) :planned)
        date-map (deref (date-map-ref w))
        dates (my-k date-map)
        start-date (first dates)
        end-date (last dates)]
    (if (or (nil? start-date) (nil? end-date))
      0
      (let [s-time (.getMillis start-date)
            e-time (.getMillis end-date)
            delta (Math/abs (- e-time s-time))
            num-days (/ delta 1000.0 60.0 60.0 24.0)]
        num-days))))

(defn fraction-of
  "Returns the fraction that a date is in a workable"
  [workable date]
  (let [date-map (deref (date-map-ref workable))
        [start-date end-date] (:planned date-map)]
    (traj/fraction-of [start-date end-date] date)))

(defn clamp-date
  "This clamps a date to a workable's date range."
  [workable date]
  (let [date-map (deref (date-map-ref workable))
        [start-date end-date] (:planned date-map)]
    (traj/clamp-date [start-date end-date] date)))

(defn null-traj-f
  "This takes a seq of date ranges and returns 0 for each one"
  [date-ranges]
  (map (fn [_] 0) date-ranges))

(defn null-named-traj-fn
  "Takes a seq of date ranges and returns {}"
  [date-ranges]
  {})

(defn set-named-traj-fn
  "Sets one of the named-traj-fn's for a w"
  [w k f]
  (let [traj-map-ref (named-traj-map-ref w)
        new-traj-map (assoc @traj-map-ref k f)]
    (ref-set traj-map-ref new-traj-map)))

; TODO: Add set-traj-f as well

(defn traj-f
  "Returns the traj-f associated with a particular key `k` for a workable"
  [workable k]
  (let [traj-f (k (:traj-f workable))]
    (if traj-f
      traj-f
      null-traj-f)))

(defn named-traj-fn
  "Returns the named-traj-fn associated with a particular key `k` for a workable"
  [workable k]
  (let [named-traj-fn (k (deref (named-traj-map-ref workable)))]
    (if named-traj-fn 
      named-traj-fn
      null-named-traj-fn)))

