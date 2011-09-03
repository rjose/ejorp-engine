(ns ejorp.nouns.project
  (:use [ejorp.protocols.workable :as workable]))

;; A project may be created without too much detail. Aside from the name and
;; id, the interesting pieces are the `date-map` and the `named-traj-f`.
;; These will be maps that we can persist in a database. We'll use these in the
;; same way: set and get by keyword.
(defrecord Project [id name date-map named-traj-f])

;; Project's implement the `Workable` protocol
(extend-type Project
  workable/Workable
  (date-map [w] (:date-map w))
  (named-traj-f [w] (:named-traj-f w)))

;(defn add-resource-req
;  "Specifies load estimates for a project"
;  [project load]
;  (let 
;    [cur-est (:est-load project)     
;     new-load (merge cur-est load)]
;    (assoc project :est-load new-load)))
;    
;(defn clear-resource-req
;  "Clears a resource requirement"
;  [project & roles]
;  (let [est-load (:est-load project)
;        new-est-load (apply dissoc est-load roles)]
;    (assoc project :est-load new-est-load)))
;
;(defn project-roles
;  "Returns the roles for a project"
;  [proj]
;  (keys (:est-load proj)))

; TODO: Revise this given the new functions for shifting workables. In particular, shifting a project
; should also shift the project's load trajectories
; We should also make a distinction between really shifting a project and shifting it for argument's sake
(defn shift-project
  [proj num-days]
  (let [new-dates (map #(.plusDays % num-days) (workable/get-dates proj :planned))]
    (workable/set-dates proj :planned new-dates)))


