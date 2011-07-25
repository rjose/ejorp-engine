(ns ejorp.nouns.person
  (:use clojure.set))

(defrecord Person [id name])

(defn add-roles
  "Adds a role to a person"
  [person & new-roles]
  (let [roles (union (:roles person) (set new-roles))]
    (assoc person :roles new-roles)))
    
(defn remove-role
  "Removes a role from a person"
  [{:keys [roles] :as person} role]
  (let [new-roles (remove #(= role %) roles)]
    (assoc person :roles new-roles)))
