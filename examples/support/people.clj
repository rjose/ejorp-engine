(ns examples.support.people
  (:use ejorp.nouns.person)
  (:import ejorp.nouns.person.Person))

(def rino (-> 
            (Person. 100 "Rino Jose")
            (add-roles "Node Engineer" "Rails Engineer" "SW Manager")))
(def roland (-> 
              (Person. 101 "Roland Jose")
              (add-roles "Warblade Knight")))
(def james (->
             (Person. 102 "James Simonsen")
             (add-roles "Node Engineer" "C++ Engineer")))
