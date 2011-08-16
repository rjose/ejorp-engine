(ns ejorp.nouns.project
  (:use ejorp.protocols.workable)
  (:use ejorp.util.density-functions))

;; A project may be created without too much detail. Initially, only the name and 
;; and the start/end dates are needed. As projects are started and tasks added,
;; more detail will be added to each project.
;; 
;; We still need to understand how to presist projects (as well as other things
;; that can have varying amounts of data in them)
(defrecord Project [id name])

;; ## Resource Requirements
;; The following functions allow us to update load estimates for projects. The units
;; of load are full-time-equivalents (FTE) for the life of a project.

(defn add-resource-req
  "Specifies load estimates for a project"
  [project load]
  (let 
    [cur-est (:est-load project)     
     new-load (merge cur-est load)]
    (assoc project :est-load new-load)))
    
(defn clear-resource-req
  "Clears a resource requirement"
  [project & roles]
  (let [est-load (:est-load project)
        new-est-load (apply dissoc est-load roles)]
    (assoc project :est-load new-est-load)))

(defn project-roles
  "Returns the roles for a project"
  [proj]
  (keys (:est-load proj)))

;; TODO: Revise this given the new functions for shifting workables. In particular, shifting a project
;; should also shift the project's load trajectories

;; ## Shifting projects
(defn shift-project
  [proj num-days]
  (let [new-dates (into {} (map (fn [[k v]] [k (.plusDays v num-days)]) (:planned-dates proj)))]
    (assoc proj :planned-dates new-dates)))
  
(defn set-planned-start-end
    [proj start end]
    (let [planned-dates (assoc (:planned-dates proj) :start start :end end)]
      (assoc proj :planned-dates planned-dates)))    
  

;; ## Workable Protocol
(extend-type Project
  Workable
                              
  (start-date
    [project]
    (:start (:planned-dates project)))

  (end-date
    [project]
    (:end (:planned-dates project))))
