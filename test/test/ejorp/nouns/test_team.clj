(ns test.ejorp.nouns.test-team
  (:use clojure.test)
  (:use ejorp.nouns.team, ejorp.nouns.person)
  (:import ejorp.nouns.team.Team, ejorp.nouns.person.Person)
  (:use fixtures.general))

;; Here, we're just testing that we can add and remove members from a team.
(deftest test-team-membership
  (let [team1 (add-members sw-team [rino "Node Engineer"] [james "Node Engineer"])
        team2 (remove-member team1 james)]
    (is (= #{[rino "Node Engineer"] [james "Node Engineer"] [roland "Warblade Knight"]} (team-members team1)))
    (is (= #{[rino "Node Engineer"] [roland "Warblade Knight"]} (team-members team2)))))

;; This tests that we don't have two members with the same ID.
(deftest test-unique-members
  (let [rino2 (assoc rino :some-data "Data")
        team1 (add-members sw-team [rino "SW"] [rino2 "Manager"])]
    (is (= 3 (count (team-members team1))))))

;; This is a variation of the prior test that checks that when a member with
;; the same ID as another member is added to a team, that member replaces the
;; existing member.
(deftest test-update-team-member
  (let [rino2 (assoc rino :some-data "Data")
        t (add-members empty-sw-team [rino "SW Engineer"])
        team1 (add-members t [rino "SW Engineer"])
        team2 (add-members t [rino2 "SW Manager"])]
    (is (= #{[rino "SW Engineer"]} (team-members t)))
    (is (= #{[rino "SW Engineer"]} (team-members team1)))
    (is (= #{[rino2 "SW Manager"]} (team-members team2)))))

;; This tests that we can get the various team roles.
(deftest test-team-roles
  (let [team1 (add-members sw-team [rino "Node Engineer"] [james "Architect"])]
    (is (= {"Node Engineer" 1, "Architect" 1, "Warblade Knight" 1}, (primary-roles team1)))))
