(ns planning.planning)

; TODO: Use a ref for Person and Team
; TODO: Define a function for creating a Person and Team

; Create a team
(defrecord Person [id name roles])
(defrecord Team [name members])


(defn add-member
  "Adds a new member to a team"
  [team person]
  (Team. (:name team) (conj (:members team) person)))

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
                (if role-count
                  (assoc m role (inc role-count))
                  (assoc m role 1))))
            {} roles)))
   
  
; TODO: Write tests for primary roles
;       Include cases where there are no roles for someone

(def a-team (Team. "SW Team" []))
(def rino (Person. 100 "Rino Jose" ["Node Engineer" "Rails Engineer" "SW Manager"]))
(def roland (Person. 101 "Roland Jose" ["Warblade Knight"]))
(def james (Person. 102 "James Simonsen" ["Node Engineer"]))

; TODO: Create a way to add multiple members to a team at once
(def new-team (add-member (add-member a-team rino) james))