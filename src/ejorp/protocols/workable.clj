(ns ejorp.protocols.workable)

(defprotocol Workable
  (set-planned-dates [workable start end])
  (start-date [workable])
  (end-date [workable]))

(defn duration
  "Computes the duration of a Workable"
  [workable]
  (let [s-time (.getTime (start-date workable))
        e-time (.getTime(end-date workable))
        delta (Math/abs (- e-time s-time))
        num-days (/ delta 1000.0 60.0 60.0 24.0)]
    num-days))

(defn fraction-of
  "Returns the fraction that a date is in a workable"
  [workable d]
  (let [s (start-date workable)
        e (end-date workable)
        s-time (.getTime s)
        e-time (.getTime e)
        d-time (.getTime d)]
    (if (or (< d-time s-time) (> d-time e-time))
      nil
      (/ (- d-time s-time) (- e-time s-time)))))

(defn clamp-date
  "This clamps a date to a workable's date range."
  [workable date]
  (let [start (start-date workable)
        end (end-date workable)]
    (cond
      (.before date start) start
      (.after date end) end
      :else date)))
