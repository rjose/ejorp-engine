(ns test.ejorp.scenarios.test-project-loading
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use ejorp.nouns.team, ejorp.nouns.person, ejorp.nouns.project)
  (:use ejorp.reports.loading)
  (:require [ejorp.protocols.workable :as workable])
  (:use ejorp.util.date)
  (:import ejorp.nouns.team.Team, ejorp.nouns.person.Person, ejorp.nouns.project.Project)
  (:use fixtures.general))


; TODO: Enable the updating of a load computation by shifting a project out in time
; Project loadings can be computed ahead of time and then manipulated. Actually, this should be doable on the client as well

        
; We should have the sense of project priority
; We should be able to estimate project overload to hold dates
; We should be able to compute project breakpoints to preserve resource loading
; TODO: Ensure the planned start < planned finish
; TODO: Test the case where the planned start and finish are the same

; TODO: Write tests for primary roles
;       Include cases where there are no roles for someone
; TODO: Allow specification of load distribution
; TODO: Add a function that can determine resource availability. We should do one version greedily
; Another version might try to be smarter about how to allocate someone who can do multiple things

;; ## Setup

;; ## Loading Estimates
;(deftest test-loading-estimates
;  (let [proj1 (Project. 1000 "Project 1" {} {})
;        proj2 (add-resource-req proj1 {"Node Engineer" 1.5})
;        proj3 (add-resource-req proj2 {"QA" 0.25})
;        proj4 (clear-resource-req proj3 "QA")]
;    (is (= {"Node Engineer" 1.5, "QA" 0.25} (:est-load proj3)))
;    (is (= {"Node Engineer" 1.5} (:est-load proj4)))))
;
;(deftest test-project-roles
;  (let [roles (project-roles jupiter)]
;    (is (= #{"Node Engineer" "QA"} (set roles)))))
;
;
;;; ## Load Computation
;(deftest test-loading-computation
;  (let [loading (project-role-loading jupiter "QA" ranges1)]
;    (is (approx= 0.085 (nth loading 0) 0.01))
;    (is (approx= 0.16 (nth loading 1) 0.01))))
;
;
;(deftest test-project-loading
;  (let [loading (project-loading jupiter ranges1)
;        node-eng-load (loading "Node Engineer")
;        qa-load (loading "QA")]
;    (is (approx= 0.97 (nth node-eng-load 1) 0.01))
;    (is (approx= 0.16 (nth qa-load 1) 0.01))))
;
;;; ## Shifting projects
;(deftest test-shift-project
;  (let [num-days 7
;        orig-dates (workable/get-dates jupiter :planned)
;        shifted-proj (shift-project jupiter num-days)
;        new-dates (workable/get-dates shifted-proj :planned)]
;    (is (= (first new-dates) (.plusDays (first orig-dates) num-days)))
;    (is (= (last new-dates) (.plusDays (last orig-dates) num-days)))))

