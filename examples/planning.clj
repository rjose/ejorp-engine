(ns examples.planning  
  (:use ejorp.util.date)
  (:use examples.support.people, examples.support.teams, examples.support.projects)  
  (:use ejorp.reports.loading))

(def ranges-1 [[(str-to-date "2011-08-01") (str-to-date "2011-09-01")] [(str-to-date "2011-09-01") (str-to-date "2011-10-30")]])
(def j-loading (project-loading jupiter ranges-1))
(def avail (resource-availability sw-team j-loading))
    