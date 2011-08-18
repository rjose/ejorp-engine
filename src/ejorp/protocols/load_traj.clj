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

(defn make-uniform-load-traj
  "Constructs a load-traj function with uniform density over a time period"
  [scale [start-date end-date]]
  (let [density-f (scale-density-fn scale uniform-density)
        traj-f (partial load-traj start-date end-date density-f)]
    traj-f))

; This is a convenience function for initial planning
(defn make-composite-uniform-load-traj
  "Constructs a map of roles to uniform load trajectories"
  [scale-map date-range]
  (into {} (map (fn [[role scale]] [role (make-uniform-load-traj scale date-range)]) scale-map)))

(defn traj-seq
  "Computes a seq of load trajectory values given a load-traj function and a seq of date ranges"
  [load-traj date-ranges]
  (map load-traj date-ranges))

(defn build-load-traj-f
  "Builds a function that returns the loading trajectory for all roles"
  [load-map]
  (fn [date-ranges]
    (into {} (map (fn [[role traj-f]] [role (traj-seq traj-f date-ranges)]) load-map))))  


;; Date shift functions
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


