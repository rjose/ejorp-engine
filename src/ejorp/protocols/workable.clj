(ns ejorp.protocols.workable
  (:require [ejorp.protocols.traj :as traj]))

;; Workables are things that require effort to be completed. The protocol itself
;; is based on getting refs to date-maps and named-traj-f's. The named-traj-f's
;; can be used in the `traj` library.
(defprotocol Workable
  (date-map [w])
  (named-traj-f [w]))

;; #### set-dates
;; This basically updates the date-map for a workable to have a new set of dates.
;; Usually, `dates` come in pairs (a start and an end). However, there can be an
;; arbitrary number of them. We have the following conventions:
;;
;; * **planned**: These dates are usually the first ones specified for a workable. 
;; These dates are the start and end for the entire workable.
;; * **in-play**: These dates are associated with live data. These are the best guesses given project data.
;; * **actual**: These dates are associated with what actually occured.
;; * **baseline-YYYY-MM-DD**: These are the dates that were `in-play` dates at the time of the baseline.
(defn set-dates
  "This sets the dates for a workable for a given key `k`"
  [w k dates]
  (let [new-date-map (assoc (date-map w) k dates)]
    (assoc w :date-map new-date-map)))

;; #### get-dates
;; This is the getter for dates by key.
(defn get-dates
  "This gets the dates for a given key `k`"
  [w k]
  (k (date-map w)))

;; #### duration
;; This returns the duration in days of a workable. The key `k` is associated with the `date-map` keys.
;; If the key is not present, we default to `:planned` dates.
(defn duration
  "Computes the duration of a w"
  [w & k]
  (let [my-k (if k (first k) :planned)
        dates (my-k (date-map w))
        start-date (first dates)
        end-date (last dates)]
    (if (or (nil? start-date) (nil? end-date))
      0
      (let [s-time (.getMillis start-date)
            e-time (.getMillis end-date)
            delta (Math/abs (- e-time s-time))
            num-days (/ delta 1000.0 60.0 60.0 24.0)]
        num-days))))

;; #### fraction-of
;; This computes the fraction from start to end of a workable.
; TODO: We should be able to specify a date-map-ref key
; TODO: When we add shelving/interrupting of workables, this needs to be smart enough to take that into account
(defn fraction-of
  "Returns the fraction that a date is in a workable"
  [workable date]
  (let [date-map (date-map workable)
        [start-date end-date] (:planned date-map)]
    (traj/fraction-of [start-date end-date] date)))

;; #### clamp-date
;; This clamps a date to a date range for a workable.
; TODO: We should be able to specify a date-map-ref key
(defn clamp-date
  "This clamps a date to a workable's date range."
  [workable date]
  (let [date-map (date-map workable)
        [start-date end-date] (:planned date-map)]
    (traj/clamp-date [start-date end-date] date)))

;; #### null-named-traj-fn
;; This just returns an empty map.
(defn null-named-traj-fn
  "Takes a seq of date ranges and returns {}"
  [date-ranges]
  {})

;; #### set-named-traj-f
;; This enables us to manage trajectory functions. We use the following naming conventions:
;;
;; * **planned-by-role**: This constructs traj's for loading requirements by role.
;; * **in-play-by-role**: This constructs traj's for loading based on live data
;; * **actual-by-role**: This constructs traj's for loading based on what actually happened
;; * **baseline-YYYY-MM-DD-by-role**: This constructs traj's for a given baseline
;;
;; In addition to these, we'll have `by-team`, `by-assignee` variants.
(defn set-named-traj-f
  "Sets one of the named-traj-fn's for a w"
  [w k f]
  (let [new-traj-map (assoc (named-traj-f w) k f)]
    (assoc w :named-traj-f new-traj-map)))

;; #### add-traj-f
;; This allows us to add a traj-f to a named-traj-f for a given key.
(defn add-traj-f
  "Adds a traj-f to a named-traj-f for a workable"
  [w k f]
  (let [my-named-traj-f (k (named-traj-f w))]
    (set-named-traj-f w k (merge my-named-traj-f f))))

;; #### remove-traj-f
;; This removes traj-f's with the given names from a named-traj-f for a given key.
(defn remove-traj-f
  "Removes traj-f from the named-traj-f of a workable"
  [w k names]
  (let [my-named-traj-f (k (named-traj-f w))]
    (set-named-traj-f w k (apply dissoc my-named-traj-f names))))

(defn traj-names
  [w k]
  (keys (k (named-traj-f w))))

;; #### named-traj-fn
;; This returns a named-traj-fn for a particular key `k`.
(defn named-traj-fn
  "Returns the named-traj-fn associated with a particular key `k` for a workable"
  [workable k]
  (let [named-traj-f (k (named-traj-f workable))]
    (if named-traj-fn 
      (traj/make-named-traj-fn named-traj-f)
      null-named-traj-fn)))

