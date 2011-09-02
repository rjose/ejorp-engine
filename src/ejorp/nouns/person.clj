;; Right now, we're not doing too much with person. This type will become more important
;; as we start assigning people to projects and tasks.
(ns ejorp.nouns.person
  (:use clojure.set))

(defrecord Person [id name])
