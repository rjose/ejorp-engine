(ns test.ejorp.nouns.team
  (:use clojure.test)
  (:use ejorp.nouns.team, ejorp.nouns.person)
  (:import ejorp.nouns.team.Team, ejorp.nouns.person.Person))

(def team (Team. "Alpha Team"))

(def rino (Person. 101 "Rino Jose"))
(def borvo (Person. 102 "Borvo Borvison"))

(deftest test-team-membership
  (let [team1 (add-members team rino borvo)
        team2 (remove-member team1 borvo)]
    (is (= #{rino borvo} (:members team1)))
    (is (= #{rino} (:members team2)))))

; TODO: Test that you can't add two members with the same ID

(deftest test-team-roles
  (let [rino2 (add-roles rino "Node Engineer" "SW Manager")
        borvo2 (add-roles borvo "Node Engineer" "QA")
        team1 (add-members team rino2 borvo2)]
    (is (= {"Node Engineer" 2}, (primary-roles team1)))
    (is (= #{"Node Engineer" "SW Manager" "QA"}, (team-roles team1)))))