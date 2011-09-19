(ns test.ejorp.nouns.test-project
  (:use clojure.test)
  (:use [ejorp.nouns.project :as project])
  (:import ejorp.nouns.project.Project)
  (:use fixtures.general))

(deftest test-mark-complete
  (let [completed-jupiter (project/mark-complete jupiter)]
    (is (= true (:is-complete (:completion-info completed-jupiter))))
    (is (not= true (:is-complete (:completion-info jupiter))))))

(deftest test-mark-incomplete
  (let [incomplete-jupiter (project/mark-incomplete jupiter)]
    (is (= false (:is-complete (:completion-info incomplete-jupiter))))
    (is (nil? (:is-complete (:completion-info jupiter))))))
