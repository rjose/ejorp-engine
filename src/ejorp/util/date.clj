(ns ejorp.util.date
  (import org.joda.time.DateTime))

(def date-parser (java.text.SimpleDateFormat. "yyyy-MM-dd"))
(defn str-to-date 
  "Converts a string to a date"
  [s]
  (DateTime. (.parse date-parser s)))

