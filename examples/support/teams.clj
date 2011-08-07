(ns examples.support.teams
  (:use examples.support.people)
  (:use ejorp.nouns.team)
  (:import ejorp.nouns.team.Team))

(def empty-sw-team (Team. 10 "SW Team"))
(def sw-team (add-members empty-sw-team rino roland))
