(ns planning.planning
  (:use clojure.set))

; TODO: Use a ref for Person and Team
; TODO: Define a function for creating a Person and Team

(defrecord Person [id name roles])
(defrecord Team [name members])
(defrecord Project [name key-dates])

(defprotocol Workable
  (workable-start [workable])
  (workable-end [workable]))




(defn add-members
  [team & new-members]
  (let [members (union (:members team) (set new-members))]
    (Team. (:name team) members)))
  
(defn remove-member
  "Removes a member from a team"
  [{:keys [members] :as team} {:keys [id]}]
  (let [new-members (remove #(= id (:id %)) members)]
    (Team. (:name team) new-members)))


(defn primary-roles
  "Returns a count of primary roles for a team"
  [{:keys [members]}]
  (let [roles (map #(first (:roles %)) members)]
    (reduce (fn [m role]
              (let [role-count (m role)]
                (if (nil? role-count)
                  (assoc m role 1)
                  (assoc m role (inc role-count)))))
            {} roles)))

(defn team-roles 
  "Returns a set of all the roles that team members can play"
  [{:keys [members] :as team}]
  (let [roles (flatten (map #(:roles %) members))]
    (set roles)))

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

(def date-parser (java.text.SimpleDateFormat. "yyyy-MM-dd"))
(defn str-to-date 
  "Converts a string to a date"
  [s]
  (.parse date-parser s))

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
   
(extend-type Project
  Workable
  (workable-start [proj]
                  (start-date proj))
  (workable-end [proj]
                (end-date proj)))

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

; TODO: Create a function that can create density functions. These should be memoized.
(defn uniform-density
  "Returns the cumulative value between two points in [0, 1]"
  [start end]
  (- end start))

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

; TODO: Enable the updating of a load computation by shifting a project out in time
; Project loadings can be computed ahead of time and then manipulated. Actually, this should be doable on the client as well

        
; We should have the sense of project priority
; We should be able to estimate project overload to hold dates
; We should be able to compute project breakpoints to preserve resource loading
; TODO: Ensure the planned start < planned finish
; TODO: Test the case where the planned start and finish are the same

; TODO: Write tests for primary roles
;       Include cases where there are no roles for someone
; TODO: Add functions to give historical data on how much effort was spent
; TODO: Allow specification of load distribution

(def rino (Person. 100 "Rino Jose" ["Node Engineer" "Rails Engineer" "SW Manager"]))
(def roland (Person. 101 "Roland Jose" ["Warblade Knight"]))
(def james (Person. 102 "James Simonsen" ["Node Engineer"]))
(def a-team (Team. "SW Team" #{}))

(def new-team (add-members a-team rino roland))

(def jupiter (ref (Project. "Jupiter" {:planned-start (str-to-date "2011-07-31"), :planned-finish (str-to-date "2011-10-30")})))
(dosync (ref-set jupiter (add-resource-req @jupiter {"Node Engineer" 1.5, "QA" 0.25})))

(def ranges-1 [[(str-to-date "2011-08-01") (str-to-date "2011-09-01")] [(str-to-date "2011-09-01") (str-to-date "2011-10-30")]])

; TODO: Add a function that can determine resource availability. We should do one version greedily
; Another version might try to be smarter about how to allocate someone who can do multiple things
