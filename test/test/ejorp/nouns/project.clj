(ns test.ejorp.nouns.project
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use ejorp.nouns.team, ejorp.nouns.person, ejorp.nouns.project)
  (:require [ejorp.protocols.workable :as workable])
  (:use ejorp.util.date)
  (:import ejorp.nouns.team.Team, ejorp.nouns.person.Person, ejorp.nouns.project.Project))

;; ## Setup
(def rino (-> 
            (Person. 100 "Rino Jose")
            (add-roles "Node Engineer" "Rails Engineer" "SW Manager")))
(def roland (-> 
              (Person. 101 "Roland Jose")
              (add-roles "Warblade Knight")))
(def james (->
             (Person. 102 "James Simonsen")
             (add-roles "Node Engineer" "C++ Engineer")))

(def team (-> 
            (Team. "SW Team")
            (add-members rino roland)))

(def jupiter (ref (-> (Project. "Jupiter")
                   (workable/set-planned-dates (str-to-date "2011-07-31") (str-to-date "2011-10-30"))
                   (add-resource-req {"Node Engineer" 1.5, "QA" 0.25}))))

(def ranges-1 [[(str-to-date "2011-08-01") (str-to-date "2011-09-01")] [(str-to-date "2011-09-01") (str-to-date "2011-10-30")]])

;; ## Loading Estimates
(deftest test-loading-estimates
  (let [proj1 (Project. "Project 1")
        proj2 (add-resource-req proj1 {"Node Engineer" 1.5})
        proj3 (add-resource-req proj2 {"QA" 0.25})
        proj4 (clear-resource-req proj3 "QA")]
    (is (= {"Node Engineer" 1.5, "QA" 0.25} (:est-load proj3)))
    (is (= {"Node Engineer" 1.5} (:est-load proj4)))))

(deftest test-project-roles
  (let [roles (project-roles @jupiter)]
    (is (= #{"Node Engineer" "QA"} (set roles)))))


;; ## Load Computation
(deftest test-loading-computation
  (let [loading (project-role-loading @jupiter "QA" ranges-1)]
    (is (approx= 0.085 (nth loading 0) 0.01))
    (is (approx= 0.16 (nth loading 1) 0.01))))


(deftest test-project-loading
  (let [loading (project-loading @jupiter ranges-1)
        node-eng-load (loading "Node Engineer")
        qa-load (loading "QA")]
    (is (approx= 0.97 (nth node-eng-load 1) 0.01))
    (is (approx= 0.16 (nth qa-load 1) 0.01))))
    