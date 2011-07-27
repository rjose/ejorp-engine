(ns ejorp.nouns.project
  (:use ejorp.protocols.workable))

;; A project may be created without too much detail. Initially, only the name and 
;; and the start/end dates are needed. As projects are started and tasks added,
;; more detail will be added to each project.
;; 
;; We still need to understand how to presist projects (as well as other things
;; that can have varying amounts of data in them)


; TODO: Move this to a utils package
; TODO: Create a function that can create density functions. These should be memoized.
(defn uniform-density
  "Returns the cumulative value between two points in [0, 1]"
  [start end]
  (- end start))

(defrecord Project [name])

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

;; ## Load Computation
;; These functions compute the loading of a project over time. They use "density functions"
;; to estimate the required effort profile.
;; 
;; These functions naturally decompose into smaller functions that can be executed in parallel.

(defn project-role-loading
  "Returns the loading for a role over a seq of date ranges"
  [proj role date-ranges]
  ; We'll default to uniform density for now, but this should come from the role
  (let [density-f uniform-density
        total-role-loading ((:est-load proj) role)
        date-ranges-as-fractions (for [r date-ranges]  (map (partial fraction-of proj) r))
        normalized-values (for [r date-ranges-as-fractions] (apply density-f r)) 
        ]
    (map #(* total-role-loading %) normalized-values)))



(defn project-loading
  "Returns the loading of a project by role over a seq of date-ranges.
  Does the docstring need to be multiple lines?"
  [proj date-ranges]
  (let [roles (project-roles proj)        
        role-loading (for [r roles] (project-role-loading proj r date-ranges)) ; NOTE: This can be parallelized 
        ]
    (zipmap roles role-loading)))


; TODO: Move to workable?
(defn clamp-date
  "This clamps a date to the project's date range."
  [proj date]
  (let [start (start-date proj)
        end (end-date proj)]
    (cond
      (.before date start) start
      (.after date end) end
      :else date)))

;; ## Workable Protocol
(extend-type Project
  Workable
  (set-planned-dates 
    [proj start end]
    (let [planned-dates (assoc (:planned-dates proj) :start start :end end)]
      (assoc proj :planned-dates planned-dates)))    
                              
  (start-date
    [project]
    (:start (:planned-dates project)))

  (end-date
    [project]
    (:end (:planned-dates project))))
