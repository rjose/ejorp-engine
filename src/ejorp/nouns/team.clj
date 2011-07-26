;; Teams need to be created dynamically from the Person collection.
;; For instance, if a user updates their primary role, the info
;; won't be propagated automatically to the team.
;;
;; We'll have to think this through a little more.
(ns ejorp.nouns.team
  (:use clojure.set))

(defrecord Team [name])

(defn add-members
  [team & new-members]
  (let [updated-members (reduce (fn [m v] (assoc m (:id v) v)) (:members team) new-members)]
    (assoc team :members updated-members)))

(defn remove-member
  "Removes a member from a team"
  [{:keys [members] :as team} {:keys [id]}]
  (let [updated-members (dissoc members id)]
    (assoc team :members updated-members)))
    
(defn primary-roles
  "Returns a map of the 'primary role' to 'number available' for a team"
  [{:keys [members]}]
  (let [roles (map #(first (:roles %)) (vals members))]
    (reduce (fn [m role]
              (let [role-count (m role)]
                (if (nil? role-count)
                  (assoc m role 1)
                  (assoc m role (inc role-count)))))
            {} roles)))

(defn team-members
  "Returns the members of a team"
  [team]
  (set (vals (:members team))))

(defn team-roles 
  "Returns a set of all the roles that team members can play"
  [{:keys [members] :as team}]
  (reduce union (map #(:roles %) (vals members))))
  