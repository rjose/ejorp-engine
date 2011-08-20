;; The load-traj module defines functions for creating and manipulating
;; "load trajectories". These are maps of roles to sequences of effort.
;; Each effort value corresponds to a specific date range. 
;; 
;; Here's an example of a load trajectory:
;; 
;; `{"Node Engineer" (0.51 0.97), "QA" (0.08 0.16)}`
;; 
(ns ejorp.protocols.load-traj
  (:use ejorp.util.density-integrals))

;; ## Utility functions
(defn clamp-date
  "Ensures a `date` is not before `start-date` and not after `end-date`."
  [[start-date end-date] date ]
  (cond
    (.isBefore date start-date) start-date
    (.isAfter date end-date) end-date
    :else date))

(defn fraction-of
  "Computes the fraction of the way that `date` is in the range defined 
  by `start-date` and `end-date. The result is in the set [0.0, 1.0]"
  [[start-date end-date] date]
  (let [[s-time e-time d-time] (map #(.getMillis %) [start-date end-date date])]
    (cond
      (< d-time s-time) 0.0
      (> d-time e-time) 1.0
      :else (/ (- d-time s-time) (- e-time s-time)))))

;; ## Load Trajectory Functions
;; "Generic load trajectory function. This is meant to be used with 'partial' to construct trajectory functions"

;; #### load-traj
;; This is meant to be used with `partial` to construct a trajectory function from
;; a `density-integral`. This function takes a `start-date` and `end-date` to define
;; the bounds of the density-integral.
;;
;; The `interval` is a seq of the start and end dates for a specific interval. Typically,
;; this is the argument that the partial should be applied to.
(defn load-traj
  "Constructs a load-traj function from a density-integral."
  [start-date end-date density-integral interval]
  (let [frac (partial fraction-of [start-date end-date])
        [start-frac end-frac] (map frac interval)]
    (density-integral start-frac end-frac)))

;; #### make-uniform-load-traj
;; This is a convenience function that constructs a load-traj function based on
;; a uniform density integral over a given period of time. The `scale` argument 
;; scales the integral to that value. For instance 
;; 
;; `(make-uniform-load-traj 3 [start-date end-date])`
;; 
;; will return a load-traj function whose total integrated value would be 3.
(defn make-uniform-load-traj
  "Constructs a load-traj function with uniform density over a time period"
  [scale [start-date end-date]]
  (let [density-f (scale-density-integral scale uniform-density-integral)
        traj-f (partial load-traj start-date end-date density-f)]
    traj-f))

;; #### uniform-load-traj-rolemap
;; This is a convenience function that takes a `scale-map` like
;; `{"SW" 2.0, "QA" 0.5}` and a date-range and returns a map of 
;; roles to uniform-load-traj functions.
(defn uniform-load-traj-rolemap
  "Constructs a map of roles to uniform load-traj functions"
  [scale-map date-range]
  (into {} (map (fn [[role scale]] [role (make-uniform-load-traj scale date-range)]) scale-map)))

;; #### build-load-traj-f
;; This is a convenience function that allows us to convert a load-traj-rolemap
;; into a function that takes a seq of date ranges and return a map of
;; roles to loading values. For instance
;; 
;;     `(def traj-f (build-load-traj-f rolemap))`
;;     `(traj-f date-ranges) => {"Node Engineer" (0.51 0.97), "QA" (0.08 0.16)}`
(defn build-load-traj-f
  "Builds a function that returns the loading trajectory for all roles"
  [rolemap]
  (let [traj-seq (fn [load-traj date-ranges] (map load-traj date-ranges))]
    (fn [date-ranges]
      (into {} (map (fn [[role traj-f]] [role (traj-seq traj-f date-ranges)]) rolemap)))))


;; ##Date shift functions
(defn shift-date-range
  [date-range num-days]
  (map #(.minusDays % num-days) date-range))

(defn shift-date-ranges
  [date-ranges num-days]
  (map #(shift-date-range % num-days) date-ranges))

(defn shift-date-ranges-f
  "Creates a function that shifts date ranges by num-days"
  [num-days]
  (fn [date-ranges] (shift-date-ranges date-ranges num-days)))

(defn shift-traj-f
  "Returns a new loading trajectory shifted in time by num-days"
  [workable-traj-f num-days]
  (comp workable-traj-f (shift-date-ranges-f num-days)))


