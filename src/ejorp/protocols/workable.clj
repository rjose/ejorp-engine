;; Workables are things that require effort to be completed. They have dates
;; associated with them such as a planned start and end.  They also have
;; trajectories associated with them that indicate things like loading over
;; time.
;;
;; We have a few naming conventions for dates:
;;
;; * **planned**: These dates are usually the first ones specified for a workable. 
;; These dates are the start and end for the entire workable.
;; * **in-play**: These dates are associated with live data. These are the best
;; guesses given project data.
;; * **actual**: These dates are associated with what actually occured.
;; * **baseline-YYYY-MM-DD**: These are the dates that were `in-play` dates at
;; the time of the baseline.
(ns ejorp.protocols.workable
  (:require [ejorp.protocols.traj :as traj]))

(defprotocol Workable
  (date-map [w])
  (named-traj-f [w]))

(defn set-dates
  "Sets the dates for a workable for a given key `k`. Dates typically come
  in pairs (a start and an end). "
  [w k dates]
  (let [new-date-map (assoc (date-map w) k dates)]
    (assoc w :date-map new-date-map)))

(defn get-dates
  "Gets the dates for a given key `k`"
  [w k]
  (k (date-map w)))

(defn duration
  "Computes the duration in days of a workable for a given key `k`. If the key
  is not present, we default to `:planned`."
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

; TODO: We should be able to specify a date-map-ref key
; TODO: When we add shelving/interrupting of workables, this needs to be smart
; enough to take that into account
(defn fraction-of
  "Returns the fraction that a date is within [start, end] of a workable."
  [workable date]
  (let [date-map (date-map workable)
        [start-date end-date] (:planned date-map)]
    (traj/fraction-of [start-date end-date] date)))

; TODO: We should be able to specify a date-map-ref key
(defn clamp-date
  "Clamps a date to a workable's date range."
  [workable date]
  (let [date-map (date-map workable)
        [start-date end-date] (:planned date-map)]
    (traj/clamp-date [start-date end-date] date)))

(defn null-traj-fn
  [date-ranges]
  {})

; TODO: In addition to these, we should have `by-team`, `by-assignee` variants.
(defn set-named-traj-f
  "Sets one of the traj-fn's for a w. We use the following naming convetions.

   * **planned-by-role**: This constructs traj's for loading requirements by role
   * **in-play-by-role**: This constructs traj's for loading based on live data
   * **actual-by-role**: This constructs traj's for loading based on what actually happened
   * **baseline-YYYY-MM-DD-by-role**: This constructs traj's for a given baseline "
  [w k f]
  (let [new-traj-map (assoc (named-traj-f w) k f)]
    (assoc w :named-traj-f new-traj-map)))

(defn add-traj-f
  "Adds a traj-f to a named-traj-f for a workable"
  [w k f]
  (let [my-named-traj-f (k (named-traj-f w))]
    (set-named-traj-f w k (merge my-named-traj-f f))))

(defn remove-traj-f
  "Removes traj-f from the named-traj-f of a workable"
  [w k names]
  (let [my-named-traj-f (k (named-traj-f w))]
    (set-named-traj-f w k (apply dissoc my-named-traj-f names))))

(defn traj-names
  "Gets the names of the named-traj-f with key `k` for a workable."
  [w k]
  (keys (k (named-traj-f w))))

(defn traj-fn
  "Returns the traj-fn associated with a particular key `k` for a workable"
  [workable k]
  (let [named-traj-f (k (named-traj-f workable))]
    (if traj-fn 
      (traj/make-traj-fn named-traj-f)
      null-traj-fn)))

(defn shift-workable
  "Shifts the dates with key `k` of a workable by some number of days."
  [w k num-days]
  (let [new-dates (map #(.plusDays % num-days) (get-dates w k))]
    (set-dates w k new-dates)))

(defn total-loading-by-role
  "Returns a traj-fn that computes the total loading across a set of workables
  by some key.
   
  This function gives us a way to view the aggregate loading by role across a
  number of workables. Technically, it can sum across any named-traj for a set
  of workables, but the normal case is to sum by role."
  [workables k]
  (let [traj-fns (map #(traj-fn % k) workables)]
    (traj/sum-traj-fns traj-fns)))

(defn loading-by-workable
  "Returns a traj-fn for computing the total loading by workable.
  
   This function gives us a way to sum across the efforts of workables and
   slice the result by workable.  We do this by squashing each of the workable
   traj-fns and then creating a new traj-fn based on the workable name."
  [workable-map k]
  (let [squash-workable (fn [[name w]] [name (traj/squash-traj-fn (traj-fn w k))]) 
        squashed-named-traj-f (into {} (map squash-workable workable-map))]
    (traj/make-traj-fn squashed-named-traj-f)))
