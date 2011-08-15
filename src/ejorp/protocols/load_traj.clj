(ns ejorp.protocols.load-traj)

(defn clamp-date
  [start-date end-date date]
  (cond
    (.isBefore date start-date) start-date
    (.isAfter date end-date) end-date
    :else date))

(defn fraction-of
  [start-date end-date date]
  (let [[s-time e-time d-time] (map #(.getMillis %) [start-date end-date date])]
    (cond
      (< d-time s-time) 0.0
      (> d-time e-time) 1.0
      :else (/ (- d-time s-time) (- e-time s-time)))))

(defn uniform-density
  "Returns the cumulative value between two points in [0, 1]"
  [start end]
  (- end start))

(defn scale-density-fn
  [scale density-f]
  (fn [s e] (* scale (density-f s e))))

(defn load-traj
  "Generic load trajectory function. This is meant to be used with 'partial' to construct trajectory functions"
  [start-date end-date density-f interval-dates]
  (let [frac (partial fraction-of start-date end-date)
        [start-frac end-frac] (map frac interval-dates)]
    (density-f start-frac end-frac)))
