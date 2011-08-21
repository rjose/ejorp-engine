;; TODO: Update the docs
;; 
;; In order to compute a loading trajectory, we use the concept of a 
;; "density function". Where density should be thought of as "effort/time".
;; The integral of one of these density functions over a date range gives the
;; effort over that date range. We use this to model loading over time.
(ns ejorp.util.density-integrals)

(defn uniform-density-integral
  "Returns the cumulative value between two points in [0, 1]"
  [[start end]]
  (- end start))

(defn scale-density-integral
  [scale density-integral]
  (fn [frac-range] (* scale (density-integral frac-range))))
