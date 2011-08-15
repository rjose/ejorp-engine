(ns ejorp.nouns.team
  (:use clojure.set))

;; Teams need to be created dynamically from Person records.
;; We won't store member information in Team records. The most we'll 
;; store is an ID. Everything else will be looked up as needed for whatever
;; operations need to be done.
(defrecord Team [id name])

;; ## Team Membership

;; Members are unique with respect to their IDs. In this function, we're simply
;; building a map of memberID to member and merging it with the current team members.
(defn add-members
  "Adds/updates team members"
  [team & new-members]
  (let [updated-members (reduce (fn [m v] (assoc m (:id (first v)) v)) (:members team) new-members)]
    (assoc team :members updated-members)))

(defn remove-member
  "Removes a member from a team"
  [{:keys [members] :as team} {:keys [id]}]
  (let [updated-members (dissoc members id)]
    (assoc team :members updated-members)))

(defn team-members
  "Returns the members of a team"
  [team]
  (set (vals (:members team))))


;; ## Team Roles
    
;; We're assuming that people can play more than one role. We're also assuming
;; that a person's first role is their primary role. This may need to be defined
;; on a per-team basis at some point.
(defn primary-roles
  "Returns a map of the 'primary role' to 'number available' for a team"
  [{:keys [members]}]
  (let [roles (map #(nth % 1) (vals members))]
    (reduce (fn [m role]
              (let [role-count (m role)]
                (if (nil? role-count)
                  (assoc m role 1)
                  (assoc m role (inc role-count)))))
            {} roles)))
