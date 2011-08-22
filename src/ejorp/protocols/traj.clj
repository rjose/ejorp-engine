;; The traj module defines functions for creating and manipulating
;; trajectories. Below is a set of definitions that we'll use throughout:
;; 
;; * **density-integral**: takes frac-range and returns the integral of the 
;;   associated density function over that range
;; * **frac-range**: an interval defined by a start-frac and end-frac
;; * **date-range**: an interval defined by a start-date and an end-date
;; * **date-ranges**: a seq of date-range
;; * **traj**: a sequence of numbers each associated with a date range
;; * **traj-f**: a function that returns a traj for a seq of date-ranges
;; * **named-traj**: A map of name => traj
;; * **named-traj-f**: A map of name => traj-f
;; * **named-traj-fn**: A function that returns a named-traj for a seq of date ranges
(ns ejorp.protocols.traj
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

;; ## Trajectory Functions

;; #### make-traj-f
;; This constructs a traj-f from a density integral.
;; The `start-date` and `end-date` define the total range of this function.
(defn make-traj-f
  "Constructs a traj-f from a density integral."
  [[start-date end-date] density-integral]
  (fn [date-ranges]
    (let [frac (partial fraction-of [start-date end-date])
          frac-ranges (map (fn [d-range] (map frac d-range)) date-ranges)]
      (map density-integral frac-ranges))))

;; #### make-uniform-traj-f
;; This is a convenience function that constructs a traj-f based on
;; a uniform density integral over a given period of time. The `scale` argument 
;; scales the integral. For instance 
;; 
;; `(make-uniform-traj-f 3 [start-date end-date])`
;; 
;; will return a load-traj function whose total integrated value would be 3.
(defn make-uniform-traj-f
  "Constructs a load-traj function with uniform density over a time period"
  [scale [start-date end-date]]
  (let [density-f (scale-density-integral scale uniform-density-integral)]
    (make-traj-f [start-date end-date] density-f)))


;; #### make-uniform-named-traj-f
;; This is a convenience function that takes a `scale-map` like
;; `{"SW" 2.0, "QA" 0.5}` and a date-range and returns a map of 
;; names to uniform-traj-f.
(defn make-uniform-named-traj-f
  "Constructs a named-traj map with uniform functions."
  [scale-map date-range]
  (into {} (map (fn [[role scale]] [role (make-uniform-traj-f scale date-range)]) scale-map)))


;; #### make-named-traj-fn
;; This creates a function that can be applied to a seq of date ranges 
;; to return the associated a named-traj.
(defn make-named-traj-fn
  "Builds a function that returns the loading trajectory for all roles"
  [named-traj-f]
  (fn [date-ranges]
    (into {} (map (fn [[role traj-f]] [role (traj-f date-ranges)]) named-traj-f))))

;; ##Date shift functions

;; #### shift-date-range
;; This shifts a date range by some number of days. We shift the date range
;; into the past because the common use case is to shift trajectories "forward".
;; Shifting trajectories forward moves dates backward relative to the original
;; traj-f functions.
(defn shift-date-range
  "Shifts a date-range into the past"
  [date-range num-days]
  (map #(.minusDays % num-days) date-range))

(defn shift-date-ranges
  "Shifts a set of date ranges into the past"
  [date-ranges num-days]
  (map #(shift-date-range % num-days) date-ranges))


;; #### shift-date-ranges-f
;; This function is useful for composing with existing functions that take
;; date-ranges since it allows us to effectively shift these functions in time.
(defn shift-date-ranges-f
  "Creates a function that shifts date ranges by num-days"
  [num-days]
  (fn [date-ranges] (shift-date-ranges date-ranges num-days)))

;; #### shift-traj-f
;; This shifts a traj-f function forward in time by num-days.
;; The intent is to do things like move a project back and forth
;; by some number of days without having to recompute the traj-f function.
(defn shift-traj-f
  "Returns a new traj-f shifted in time by num-days"
  [traj-f num-days]
  (comp traj-f (shift-date-ranges-f num-days)))


