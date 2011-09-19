(ns fixtures.general
  (:use ejorp.nouns.project, ejorp.nouns.team, ejorp.nouns.person)
  (:use ejorp.util.date)
  (:use ejorp.reports.loading)
  (:use clojure.set)
  (:import ejorp.nouns.team.Team, ejorp.nouns.person.Person, ejorp.nouns.project.Project)
  (:require [ejorp.protocols.traj :as traj])
  (:require [ejorp.protocols.workable :as workable]))

(def rino (Person. 100 "Rino Jose"))
(def roland (Person. 101 "Roland Jose"))
(def james (Person. 102 "James Simonsen"))

(def empty-sw-team (Team. 10 "SW Team"))
(def sw-team (add-members empty-sw-team 
                          [rino "Node Engineer"] 
                          [roland "Warblade Knight"] 
                          [james "Node Engineer"]))

(def jul15 (str-to-date "2011-07-15"))
(def jul31 (str-to-date "2011-07-31"))
(def oct30 (str-to-date "2011-10-30"))
(def nov30 (str-to-date "2011-11-30"))

(def traj-map1 (traj/make-uniform-named-traj-f {"Node Engineer" 1.5, "QA" 1} [jul31 oct30]))
(def traj-map2 (traj/make-uniform-named-traj-f {"Node Engineer" 2.5, "QA" 0.5, 
                                                "Warblade Knight" 1.0} [jul15 nov30]))

;; TODO: We may need to rewrite these in the face of the new workable changes
(def jupiter (-> (Project. 1000 "Jupiter" {} {} {} {})
               (workable/set-named-traj-f :planned-by-role traj-map1)
               (workable/set-dates :planned [jul31 oct30])))


(def neptune (-> (Project. 1001 "Neptune" {} {} {} {})
               (workable/set-named-traj-f :planned-by-role traj-map2)
               (workable/set-dates :planned [jul15 nov30])))


(def ranges-1 [[(str-to-date "2011-08-01") (str-to-date "2011-09-01")] [(str-to-date "2011-09-01") (str-to-date "2011-10-30")]])

;; ## Date Ranges
(def ranges1 (partition 2 (map str-to-date ["2011-08-01" "2011-09-01", "2011-09-01" "2011-10-30"])))

;;; ## Project Loading
;(def jupiter-loading (project-loading jupiter ranges1))
;(def neptune-loading (project-loading neptune ranges1))
;
;;; ## Resource availability
;(def avail (resource-availability sw-team [jupiter-loading neptune-loading]))
;
;; TODO: Describe this
;(defn- normalize-unused-resources
;  [net-resources num-date-ranges]
;  (into {}
;        (for [[k v] net-resources]
;          (if (seq? v)
;            [k v]
;            [k (take num-date-ranges (repeat v))]))))  
;
;; TODO: Think about how we can apply this function to a generic project concept
;; TODO: Make project-roles more generic
;(defn net-avail
;  [team projects date-ranges]
;  (let [team-resources (primary-roles team)
;        roles (union (set (keys team-resources)) (set (mapcat project-roles projects)))
;        resources (merge (zipmap roles (repeat 0)) team-resources)
;        loadings (zipmap projects (map #(project-loading % date-ranges) projects))
;        total-loading (apply merge-with #(map + %1 %2) (vals loadings))
;        net-resources (merge-with (fn [resource loading-seq] (map #(- resource %) loading-seq)) resources total-loading)
;        norm-net-resources (normalize-unused-resources net-resources (count date-ranges))]
;    {:net-avail norm-net-resources, :loadings loadings, :date-ranges date-ranges}))
;
;(def net-avail-results (net-avail sw-team [jupiter neptune] ranges1))
;
;;(defn shift-project
;;  [{:keys [loadings date-ranges]} proj num-days]
;;  (let [loadings-without-proj (into {} (filter #(not= (:id proj) (:id (first %))) loadings))
;;        proj-dates (into {} (map (fn [[k v]] [k (.plusDays v num-days)]) (:planned-dates proj)))
;;        shifted-project (assoc proj :planned-dates proj-dates)
;;        new-loading (project-loading shifted-project date-ranges)
;;        
;;        total-loading (apply merge-with #(map + %1 %2) (conj (vals loadings-without-proj) new-loading))
;;        ]
;;    [(vals loadings-without-proj), new-loading]))
;
;;(shift-project net-avail-results neptune 10)
