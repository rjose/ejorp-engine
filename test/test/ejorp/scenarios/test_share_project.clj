(ns test.ejorp.scenarios.test-share-project
  (:use clojure.test)
  (:use [ejorp.nouns.project :as project])
  (:import ejorp.nouns.project.Project)
  (:require [ejorp.protocols.workable :as workable])
  (:use fixtures.general))

(def sw-group-id 27)
(def owner-id 412)
(def owner-id2 101)

(deftest test-share-project-with-group
  (let [shared-project (project/share-with-group jupiter sw-group-id)]
    (is (= #{27} (:group-ids (:metadata shared-project))))))

(deftest test-set-owner
  (let [owned-project (project/set-owner jupiter owner-id)]
    (is (= 412 (:owner-id (:metadata owned-project))))
    (is (nil? (:owner-id (:metadata jupiter))))))

(def saturn (-> (Project. 1002 "Saturn" {} {} {} {})
               (workable/set-named-traj-f :planned-by-role traj-map2)
               (workable/set-dates :planned [jul15 nov30])))

(def jupiter2 (project/set-owner jupiter owner-id))
(def neptune2 (project/set-owner neptune owner-id))
(def saturn2 (project/set-owner saturn owner-id2))

(def all-projects [jupiter2 neptune2 saturn2])

(deftest test-owned-by
  (let [owned-projects (project/owned-by all-projects owner-id)
        expected-ids (map :id [jupiter2 neptune2])]
    (is (= expected-ids (map :id owned-projects)))))

(deftest test-by-owner
  (let [projects-by-owner (project/by-owner all-projects)]
    (is (= {412 [neptune2 jupiter2] 101 [saturn2]} projects-by-owner))))


(deftest test-request-help
  (let [new-jupiter (project/request-help jupiter2 owner-id2)]
    (is (= #{101} (:asked-for-help (:metadata new-jupiter))))))


(def project-needing-help (project/request-help jupiter2 owner-id2))

(deftest test-request-get-help-requests
  (let [all-projects [project-needing-help saturn2]]
    (is (= [project-needing-help] (project/get-help-requests all-projects owner-id2)))))

(deftest test-clear-help-request
  (let [helped-project (project/clear-help-request project-needing-help owner-id2)]
    (is (= #{} (:asked-for-help (:metadata helped-project))))))
