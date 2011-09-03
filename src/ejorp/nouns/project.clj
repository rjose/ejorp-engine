(ns ejorp.nouns.project
  (:use [ejorp.protocols.workable :as workable]))

;; A project may be created without too much detail. Aside from the name and
;; id, the interesting pieces are the `date-map` and the `named-traj-f`.
;; These will be maps that we can persist in a database. We'll use these in the
;; same way: set and get by keyword.
(defrecord Project [id name date-map named-traj-f])

;; Projects implement the `Workable` protocol
(extend-type Project
  workable/Workable
  (date-map [w] (:date-map w))
  (named-traj-f [w] (:named-traj-f w)))

