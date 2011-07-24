(ns protocols.workable)

(defprotocol Workable
  (workable-start [workable])
  (workable-end [workable]))

(defn duration
  "Computes the duration of a Workable"
  [workable]
  (let [s-time (.getTime (workable-start workable))
        e-time (.getTime(workable-end workable))
        delta (Math/abs (- e-time s-time))
        num-days (/ delta 1000.0 60.0 60.0 24.0)]
    num-days))

(defn fraction-of-workable
  "Returns the fraction that a date is in a workable"
  [workable d]
  (let [s (workable-start workable)
        e (workable-end workable)
        s-time (.getTime s)
        e-time (.getTime e)
        d-time (.getTime d)]
    (if (or (< d-time s-time) (> d-time e-time))
      nil
      (/ (- d-time s-time) (- e-time s-time)))))
