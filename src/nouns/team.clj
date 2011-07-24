(ns nouns.team
  (:use clojure.set))

(defrecord Team [name members])

(defn add-members
  [team & new-members]
  (let [members (union (:members team) (set new-members))]
    (Team. (:name team) members)))
  
(defn remove-member
  "Removes a member from a team"
  [{:keys [members] :as team} {:keys [id]}]
  (let [new-members (remove #(= id (:id %)) members)]
    (Team. (:name team) new-members)))

(defn primary-roles
  "Returns a count of primary roles for a team"
  [{:keys [members]}]
  (let [roles (map #(first (:roles %)) members)]
    (reduce (fn [m role]
              (let [role-count (m role)]
                (if (nil? role-count)
                  (assoc m role 1)
                  (assoc m role (inc role-count)))))
            {} roles)))

(defn team-roles 
  "Returns a set of all the roles that team members can play"
  [{:keys [members] :as team}]
  (let [roles (flatten (map #(:roles %) members))]
    (set roles)))
