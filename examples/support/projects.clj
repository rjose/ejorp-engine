(ns examples.support.projects
  (:use ejorp.nouns.project)
  (:use ejorp.util.date)
  (:import ejorp.nouns.project.Project)
  (:use ejorp.protocols.workable))

(def jupiter (-> (Project. 1000 "Jupiter")
                   (set-planned-dates (str-to-date "2011-07-31") (str-to-date "2011-10-30"))
                   (add-resource-req {"Node Engineer" 1.5, "QA" 0.25})))
