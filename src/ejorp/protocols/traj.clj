;; The traj module defines functions for creating and manipulating
;; trajectories. Below is a set of definitions that we'll use throughout:
;; 
;; * **density-integral**: takes frac-range and returns the integral of the 
;;   associated density function over that range
;; * **frac-range**: an interval defined by a start-frac and end-frac
;; * **date-range**: an interval defined by a start-date and an end-date
;; * **date-ranges**: a seq of date-range
;; * **traj**: a sequence of numbers each associated with a date range
;; * **named-traj**: A map of name => traj
;; * **traj-f**: a function that returns a traj for a seq of date-ranges
;; * **named-traj-f**: A map of name => traj-f
;; * **traj-fn**: A function that returns a named-traj for a seq of date ranges
(ns ejorp.protocols.traj
  (:use ejorp.util.density-integrals)
  (:use ejorp.util.date))

;; ## Utility functions
;; These are various utility functions that apply to traj functions.  We may
;; move them to a more general place if other modules need them as well.

(defn- clamp 
  "Clamps a value to be within the given range"
  [x min max] 
  (cond (> x max) max
        (< x min) min
        :default x))

(defn clamp-date
  "Ensures a `date` is not before `start-date` and not after `end-date`."
  [[start-date end-date] date ]
  (cond (.isBefore date start-date) start-date
        (.isAfter date end-date) end-date
        :else date))

(defn fraction-of
  "Computes the fraction of the way that `date` is in the range defined 
  by `start-date` and `end-date. The result is in the set [0.0, 1.0]"
  [[start-date end-date] date]
  (let [[s-time e-time d-time] (map #(.getMillis %) [start-date end-date date])]
    (cond (< d-time s-time) 0.0
          (> d-time e-time) 1.0
          :else (/ (- d-time s-time) (- e-time s-time)))))

;; ## Trajectory Functions

(defn density-integral-to-traj-f
  "Constructs a traj-f from a density integral. The `start-date` and `end-date`
  define the total range of this function."
  [[start-date end-date] density-integral]
  (fn [date-ranges]
    (let [frac (partial fraction-of [start-date end-date])
          frac-ranges (map (fn [d-range] (map frac d-range)) date-ranges)]
      (map density-integral frac-ranges))))

(defn make-uniform-traj-f
  "Constructs a load-traj function with uniform density over a time period. The
  `scale` argument scales the integral. For instance:

    (make-uniform-traj-f 3 [start-date end-date])

  will return a load-traj function whose total integrated value would be 3."
  [scale [start-date end-date]]
  (let [density-f (scale-density-integral scale uniform-density-integral)]
    (density-integral-to-traj-f [start-date end-date] density-f)))


(defn make-uniform-named-traj-f
  "Constructs a named-traj map with uniform traj-f's from a 'scale-map' of the form:
  
    {'SW' 2.0, 'QA' 0.5} "
  [scale-map date-range]
  (into {} (map (fn [[role scale]] [role (make-uniform-traj-f scale date-range)]) scale-map)))


(defn make-traj-fn
  "This creates a function that can be applied to a seq of date ranges to
  return the associated a named-traj."
  [named-traj-f]
  (fn [date-ranges]
    (into {} (map (fn [[name traj-f]] [name (traj-f date-ranges)]) named-traj-f))))


;; ##Date shift functions

(defn shift-date-range
  "Shifts a date-range into the past.  We shift the date range into the past
  because the common use case is to shift trajectories 'forward'.  Shifting
  trajectories forward moves dates backward relative to the original traj-f
  functions."
  [date-range num-days]
  (map #(.minusDays % num-days) date-range))

(defn shift-date-ranges
  "Shifts a set of date ranges into the past"
  [date-ranges num-days]
  (map #(shift-date-range % num-days) date-ranges))


(defn shift-date-ranges-f
  "Creates a function that shifts date ranges by num-days. This function is
  useful for composing with existing functions that take date-ranges since it
  allows us to effectively shift these functions in time."
  [num-days]
  (fn [date-ranges] (shift-date-ranges date-ranges num-days)))

(defn shift-traj-f
  "Returns a new traj-f shifted in time by num-days.  The intent is to do
  things like move a project back and forth by some number of days without
  having to recompute the traj-f function."
  [traj-f num-days]
  (comp traj-f (shift-date-ranges-f num-days)))


;; ##traj-f summing functions

(defn sum-traj
  "Sums a set of trajs, returning a new traj. We assume that all trajs are the
  same length and are associated with the same date ranges."
  [& trajs]
  (if (pos? (count trajs)) (apply map + trajs) []))


(defn sum-named-traj
  "Returns the sum of named-traj's"
  [& named-trajs]
  (if (pos? (count named-trajs)) (apply merge-with sum-traj named-trajs) {}))

(defn sum-traj-fns
  "Builds a function that returns the sum of a seq of traj-fns over a seq of
  date ranges This function is used to sum multiple traj-fns together. The
  result is the sum by whatever names in each traj-fn. This will be useful in
  computing total loading by role for a set of projects (or other workables). 

  This can also be used to sum projects 'in play' and overlay a set of planned
  projects on top to see where the staff shortfalls are."
  [traj-fns]
  (fn [date-ranges] 
    (let [named-traj-fs (map (fn [traj-fn] (traj-fn date-ranges)) traj-fns)]
      (apply sum-named-traj named-traj-fs))))

(defn squash-traj-fn
  "Returns a function that sums the values across all names for a traj-fn.
  This function can be used to create a function that squashes (i.e. sums) all
  values in each date range.  This is useful in looking at an aggregate loading
  chart by project/workable."
  [traj-fn]
  (fn [date-ranges]
    (let [named-traj-f (traj-fn date-ranges)]
      (apply map + (vals named-traj-f)))))

;; ## traj-f construction functions

(defn make-traj-f-element
  "Returns a traj-fn that can be applied to a seq of date-ranges to produce a
  traj.
  
  This is used to construct a function that can be used to generate a traj-f from
  effort data. The `start-bound-date` is the reference date for this function
  and is associated with the first value in `values`. The `values` vec contains
  effort info for each consecutive day after `start-bound-date`

  This returns a function that takes a date range and returns the sum of the
  efforts within that range. The range includes the start but not the end,
  i.e., it looks like this: `[s-date, e-date)`."
  [start-bound-date values]
  (let [end-bound-rel (count values)]
    (fn [[s-date e-date]]
      (let [s-rel (clamp (subtract-dates s-date start-bound-date) 0 end-bound-rel) 
            e-rel (clamp (subtract-dates e-date start-bound-date) 0 end-bound-rel)]
        (apply + (subvec values s-rel e-rel))))))


(defn effort-data-to-traj-f
  "Retrns a traj-f based on some data. This function takes an `effort-data` map that looks like this:

    {:start-date date, :values [1 2 3 4 5 5 4 3 2 1 ]}

  and returns a traj-f defined over that data."
  [{:keys [start-date values]}]
  (let [traj-fn (make-traj-f-element start-date values)]
    (fn [date-ranges] 
      (map traj-fn date-ranges)))) 

(defn effort-data-to-named-traj-f
  "Returns a named-traj-f based on some data.  This takes effort data in the
  form of a `named-effort-map` and returns a traj-fn based on it. The effort
  map looks like this:

    {'SW' {:start-date date, 
           :values [1 2 3 4 5 5 4 3 2 1 ]}}"
  [named-effort-map]
  (let [named-traj-f (into {} (map (fn [[role effort-data]] 
                                     [role (effort-data-to-traj-f effort-data)]) named-effort-map))]
    (make-traj-fn named-traj-f)))

