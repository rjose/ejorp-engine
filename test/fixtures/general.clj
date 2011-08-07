(ns fixtures.general
  (:use ejorp.nouns.project, ejorp.nouns.team, ejorp.nouns.person)
  (:use ejorp.util.date)
  (:use ejorp.reports.loading)
  (:import ejorp.nouns.team.Team, ejorp.nouns.person.Person, ejorp.nouns.project.Project)
  (:require [ejorp.protocols.workable :as workable]))

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
            (Team. 10 "SW Team")
            (add-members rino roland)))

(def jupiter (ref (-> (Project. 1000 "Jupiter")
                   (workable/set-planned-dates (str-to-date "2011-07-31") (str-to-date "2011-10-30"))
                   (add-resource-req {"Node Engineer" 1.5, "QA" 0.25}))))

(def ranges-1 [[(str-to-date "2011-08-01") (str-to-date "2011-09-01")] [(str-to-date "2011-09-01") (str-to-date "2011-10-30")]])

(def jupiter-loading (project-loading @jupiter ranges-1))