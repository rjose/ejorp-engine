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
  (:use ejorp.util.density-integrals)
  (:use ejorp.util.date))

;; ## Utility functions
;; These are various utility functions that apply to traj functions.
;; We may move them to a more general place if other modules need
;; them as well.
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

;; #### density-integral-to-traj-f
;; This constructs a traj-f from a density integral.
;; The `start-date` and `end-date` define the total range of this function.
(defn density-integral-to-traj-f
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
    (density-integral-to-traj-f [start-date end-date] density-f)))


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


;; #### sum-traj
;; This sums a set of trajs, returning a new traj. We assume that
;; all trajs are the same length and are associated with the same
;; date ranges.
(defn sum-traj
  "Returns the sum of traj's"
  [& trajs]
  (if (pos? (count trajs)) (apply map + trajs) []))

;; #### sum-named-traj
;; This is the named version of sum-traj. 
(defn sum-named-traj
  "Returns the sum of named-traj's"
  [& named-trajs]
  (if (pos? (count named-trajs)) (apply merge-with sum-traj named-trajs) {}))

;; #### make-traj-fn
;; This is used to construct a function that can be used to generate traj's from
;; effort data. The `start-bound-date` is the reference date for this function
;; and is associated with the first value in `values`. The `values` vec contains
;; effort info for each consecutive day after `start-bound-date`
;;
;; This returns a function that takes a date range and returns the sum of the 
;; efforts within that range. The range includes the start but not the end, i.e.,
;; it looks like this: [s-date, e-date).
(defn make-traj-fn
  "Returns a traj-fn that can be applied to a seq of date-ranges to product a traj"
  [start-bound-date values]
  (let [end-bound-rel (count values)]
    (fn [[s-date e-date]]
      (let [s-rel (clamp (subtract-dates s-date start-bound-date) 0 end-bound-rel) 
            e-rel (clamp (subtract-dates e-date start-bound-date) 0 end-bound-rel)]
        (apply + (subvec values s-rel e-rel))))))


;; #### effort-data-to-traj-f
;; This function takes an `effort-data` map that looks like this:
;; 
;;   `{:start-date (str-to-date "2011-08-22"), :values [1 2 3 4 5 5 4 3 2 1 ]}`
;;
;; and returns a traj-f defined over that data.
(defn effort-data-to-traj-f
  "Retrns a traj-f based on some data"
  [{:keys [start-date values]}]
  (let [traj-fn (make-traj-fn start-date values)]
    (fn [date-ranges] 
      (map traj-fn date-ranges)))) 

;; #### effort-data-to-named-traj-f
;; This takes effort data in the form of a `named-effort-map` and returns a 
;; a named-traj-fn based on it. The effort map looks like this:
;;
;;   `{"SW" {:start-date (str-to-date "2011-08-22"), :values [1 2 3 4 5 5 4 3 2 1 ]}}`
(defn effort-data-to-named-traj-f
  "Returns a named-traj-f based on some data"
  [named-effort-map]
  (let [named-traj-f (into {} (map (fn [[role effort-data]] 
                                     [role (effort-data-to-traj-f effort-data)]) named-effort-map))]
    (make-named-traj-fn named-traj-f)))
