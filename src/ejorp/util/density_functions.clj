(ns ejorp.util.density-functions)

; TODO: Create a function that can create density functions. These should be memoized.
(defn uniform-density
  "Returns the cumulative value between two points in [0, 1]"
  [start end]
  (- end start))
