(ns ejorp.util.date)

(def date-parser (java.text.SimpleDateFormat. "yyyy-MM-dd"))
(defn str-to-date 
  "Converts a string to a date"
  [s]
  (.parse date-parser s))

