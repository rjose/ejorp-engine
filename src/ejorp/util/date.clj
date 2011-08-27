(ns ejorp.util.date
  (import org.joda.time.DateTime)
  (import org.joda.time.Days))

(def date-parser (java.text.SimpleDateFormat. "yyyy-MM-dd"))
(defn str-to-date 
  "Converts a string to a date"
  [s]
  (DateTime. (.parse date-parser s)))

(defn subtract-dates
  "Gets the number of days between two dates"
  [d1 d2]
  (.getDays (. Days daysBetween d2 d1)))
