(ns planning.planning
  (:use clojure.set))

; TODO: Use a ref for Person and Team
; TODO: Define a function for creating a Person and Team

; Create a team
(defrecord Person [id name roles])
(defrecord Team [name members])


; TODO: Make members a set
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

; TODO: Add a function to get all the roles for a team
; TODO: Add a function that can add projects
; TODO: Add a function that can determine resource availability. We should do one version greedily
; Another version might try to be smarter about how to allocate someone who can do multiple things
; We should have the sense of project priority
; We should be able to estimate project overload to hold dates
; We should be able to compute project breakpoints to preserve resource loading
; TODO: Write tests for primary roles
;       Include cases where there are no roles for someone

(def a-team (Team. "SW Team" #{}))
(def rino (Person. 100 "Rino Jose" ["Node Engineer" "Rails Engineer" "SW Manager"]))
(def roland (Person. 101 "Roland Jose" ["Warblade Knight"]))
(def james (Person. 102 "James Simonsen" ["Node Engineer"]))

(def new-team (add-members a-team rino roland))