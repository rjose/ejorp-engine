(ns examples.planning  
  (:use ejorp.util.date)
  (:use ejorp.nouns.project, ejorp.nouns.team, ejorp.nouns.person)
  (:use ejorp.reports.loading)
  (:use ejorp.protocols.workable)  
  (:use clojure.set)
  (:import ejorp.nouns.project.Project, ejorp.nouns.team.Team, ejorp.nouns.person.Person)
  )


(def rino (-> 
            (Person. 100 "Rino Jose")
            (add-roles "Node Engineer" "Rails Engineer" "SW Manager")))
(def roland (-> 
              (Person. 101 "Roland Jose")
              (add-roles "Warblade Knight")))
(def james (->
             (Person. 102 "James Simonsen")
             (add-roles "Node Engineer" "C++ Engineer")))

(def a-team (Team. 10 "SW Team"))

(def new-team (add-members a-team rino roland))

(def jupiter (-> (Project. 1000 "Jupiter")
                   (set-planned-dates (str-to-date "2011-07-31") (str-to-date "2011-10-30"))
                   (add-resource-req {"Node Engineer" 1.5, "QA" 0.25})))


(def ranges-1 [[(str-to-date "2011-08-01") (str-to-date "2011-09-01")] [(str-to-date "2011-09-01") (str-to-date "2011-10-30")]])

(def j-loading (project-loading jupiter ranges-1))

         
(def avail (resource-availability new-team j-loading))
    