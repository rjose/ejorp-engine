(ns ejorp.reports.loading
  (:use ejorp.nouns.project, ejorp.nouns.team, ejorp.nouns.person)
  (:use ejorp.util.density-functions)
  (:use ejorp.protocols.workable)
  (:use clojure.set))

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
        normalized-values (for [r date-ranges-as-fractions] (apply density-f r))]
    (map #(* total-role-loading %) normalized-values)))


(defn project-loading
  "Returns the loading of a project by role over a seq of date-ranges."
  [proj date-ranges]
  (let [roles (project-roles proj)        
        role-loading (for [r roles] (project-role-loading proj r date-ranges))]
    (zipmap roles role-loading)))

(defn p-project-loading
  "This is the parallelized version of project-loading."
  [proj date-ranges]
  (let [roles (project-roles proj)
        working-agents (for [r roles] (send (agent proj) project-role-loading r date-ranges))]
    (apply await-for 5000 working-agents)
    (zipmap roles (map deref working-agents))))


;; ## Resource Availability
(defn resource-availability
  "Computes the net resource availability for a team against a loading trajectory"
  [team loading]
  (let [team-resources (primary-roles team)
        roles (union (set (keys team-resources)) (set (keys loading)))
        resources (merge (zipmap roles (repeat 0)) team-resources)        
        net-resources (merge-with (fn [resource loading-seq] (map #(- resource %) loading-seq)) resources loading)]
    (into {} 
          (for [[k v] net-resources]
            (if (seq? v)
              [k v]
              [k (take (count loading) (repeat v))])))))
