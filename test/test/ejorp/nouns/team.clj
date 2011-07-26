(ns test.ejorp.nouns.team
  (:use clojure.test)
  (:use ejorp.nouns.team, ejorp.nouns.person)
  (:import ejorp.nouns.team.Team, ejorp.nouns.person.Person))

(def team (Team. "Alpha Team"))
(def rino (Person. 101 "Rino Jose"))
(def borvo (Person. 102 "Borvo Borvison"))

;; Here, we're just testing that we can add and remove members from a team.
(deftest test-team-membership
  (let [team1 (add-members team rino borvo)
        team2 (remove-member team1 borvo)]
    (is (= #{rino borvo} (team-members team1)))
    (is (= #{rino} (team-members team2)))))

;; This tests that we don't have two members with the same ID.
(deftest test-unique-members
  (let [rino2 (add-roles rino "Entrepreneur")
        team1 (add-members team rino rino2)]
    (is (= 1 (count (team-members team1))))))

;; This is a variation of the prior test that checks that when a member with
;; the same ID as another member is added to a team, that member replaces the
;; existing member.
(deftest test-update-team-member
  (let [rino2 (add-roles rino "Entrepreneur")
        team1 (add-members team rino)
        team2 (add-members team rino2)]
    (is (= #{rino} (team-members team1)))
    (is (= #{rino2} (team-members team2)))))

;; This tests that we can get the various team roles.
(deftest test-team-roles
  (let [rino2 (add-roles rino "Node Engineer" "SW Manager")
        borvo2 (add-roles borvo "Node Engineer" "QA")
        team1 (add-members team rino2 borvo2)]
    (is (= {"Node Engineer" 2}, (primary-roles team1)))
    (is (= #{"Node Engineer" "SW Manager" "QA"}, (team-roles team1)))))