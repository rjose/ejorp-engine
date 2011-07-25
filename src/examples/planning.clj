(ns examples.planning  
  (:use ejorp.util.date)
  (:use ejorp.nouns.project, ejorp.nouns.team, ejorp.nouns.person)
  (:use ejorp.protocols.workable)
  (:import ejorp.nouns.project.Project, ejorp.nouns.team.Team, ejorp.nouns.person.Person)
  )

; TODO: Use a ref for Person and Team
; TODO: Define a function for creating a Person and Team


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

;(def jupiter (ref (Project. "Jupiter" {:planned-start (str-to-date "2011-07-31"), :planned-finish (str-to-date "2011-10-30")})))
;(def jupiter (ref (Project. "Jupiter")))

(def jupiter (ref (-> (Project. "Jupiter")
                   (workable-set-planned-dates (str-to-date "2011-07-31") (str-to-date "2011-10-30"))
                   (add-resource-req {"Node Engineer" 1.5, "QA" 0.25}))))


(def ranges-1 [[(str-to-date "2011-08-01") (str-to-date "2011-09-01")] [(str-to-date "2011-09-01") (str-to-date "2011-10-30")]])

; TODO: Add a function that can determine resource availability. We should do one version greedily
; Another version might try to be smarter about how to allocate someone who can do multiple things
