(ns nouns.project)

; TODO: Move these to a workable package
(defprotocol Workable
  (workable-start [workable])
  (workable-end [workable]))

(defn duration
  "Computes the duration of a Workable"
  [workable]
  (let [s-time (.getTime (workable-start workable))
        e-time (.getTime(workable-end workable))
        delta (Math/abs (- e-time s-time))
        num-days (/ delta 1000.0 60.0 60.0 24.0)]
    num-days))

(defn fraction-of-workable
  "Returns the fraction that a date is in a workable"
  [workable d]
  (let [s (workable-start workable)
        e (workable-end workable)
        s-time (.getTime s)
        e-time (.getTime e)
        d-time (.getTime d)]
    (if (or (< d-time s-time) (> d-time e-time))
      nil
      (/ (- d-time s-time) (- e-time s-time)))))


; TODO: Move this to a utils package
; TODO: Create a function that can create density functions. These should be memoized.
(defn uniform-density
  "Returns the cumulative value between two points in [0, 1]"
  [start end]
  (- end start))


(defrecord Project [name key-dates])

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

(defn start-date
  "Returns start date of project. Depending on stage of project, this may
be a planned date or an actual date"
  [project]
  (:planned-start (:key-dates project)))

(defn end-date
  "Returns end date of project. Depending on stage of project, this may
be a planned date or an actual date"
  [project]
  (:planned-finish (:key-dates project)))

(defn clamp-date
  "This clamps a date to the project's date range."
  [proj date]
  (let [start (start-date proj)
        end (end-date proj)]
    (cond
      (.before date start) start
      (.after date end) end
      :else date)))

(defn project-role-loading
  "Returns the loading for a role over a seq of date ranges"
  [proj role date-ranges]
  ; We'll default to uniform density for now, but this should come from the role
  (let [density-f uniform-density
        total-role-loading ((:est-load proj) role)
        date-ranges-as-fractions (for [r date-ranges]  (map (partial fraction-of-workable proj) r))
        normalized-values (for [r date-ranges-as-fractions] (apply density-f r)) 
        ]
    (map #(* total-role-loading %) normalized-values)))

(defn project-roles
  "Returns the roles for a project"
  [proj]
  (keys (:est-load proj)))
  
(defn project-loading
  "Returns the loading of a project by role over a seq of date-ranges"
  [proj date-ranges]
  (let [roles (project-roles proj)        
        role-loading (for [r roles] (project-role-loading proj r date-ranges)) ; NOTE: This can be parallelized 
        ]
    (zipmap roles role-loading)))


(extend-type Project
  Workable
  (workable-start [proj]
                  (start-date proj))
  (workable-end [proj]
                (end-date proj)))
