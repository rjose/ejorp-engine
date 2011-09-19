;; A project may be created without too much detail. Aside from the name and
;; id, the interesting pieces are the `date-map` and the `named-traj-f`.
;; These will be maps that we can persist in a database. We'll use these in the
;; same way: set and get by keyword.
(ns ejorp.nouns.project
  (:use [ejorp.protocols.workable :as workable]))

;; Projects implement the `Workable` protocol
(defrecord Project [id name date-map named-traj-f completion-info metadata])

(extend-type Project
  workable/Workable
  (date-map [w] (:date-map w))
  (named-traj-f [w] (:named-traj-f w)))

(defn share-with-group
  "Shares a project with a LinkedIn group"
  [p group-id]
  (let [cur-groups-ids (set (:group-ids (:metadata p)))
        new-group-ids (conj cur-groups-ids group-id)
        new-metadata (assoc (:metadata p) :group-ids new-group-ids)]
    (assoc p :metadata new-metadata)))

(defn set-owner
  "Sets the ID of the user who owns the project"
  [p owner-id]
  (let [new-metadata (assoc (:metadata p) :owner-id owner-id)]
    (assoc p :metadata new-metadata)))

(defn owned-by
  "Filters projects by those owned by the specified user"
  [p-seq owner-id]
  (filter #(= owner-id (:owner-id (:metadata %))) p-seq))

(defn by-owner
  "Returns a map of 'owner ids' => 'seqs of projects' from a seq of projects"
  [p-seq]
  (reduce (fn [acc p]
            (let [owner-id (:owner-id (:metadata p))
                  owned-p (conj (acc owner-id) p)]
              (assoc acc owner-id owned-p))) 
          {} p-seq))

(defn request-help
  "Marks a project as needing help from someone"
  [p user-id]
  (let [cur-asked-for-help (set (:asked-for-help (:metadata p)))
        new-asked-for-help (conj cur-asked-for-help user-id)
        new-metadata (assoc (:metadata p) :asked-for-help new-asked-for-help)]
    (assoc p :metadata new-metadata)))

(defn get-help-requests
  "Returns a seq of projects where help has been requested of the given user"
  [p-seq helper-id]
  (filter #(contains? (:asked-for-help (:metadata %)) helper-id) p-seq))


(defn clear-help-request
  "Clears any help request needed from a user for a given project"
  [p helper-id]
  (let [cur-asked-for-help (set (:asked-for-help (:metadata p)))
        new-asked-for-help (disj cur-asked-for-help helper-id)
        new-metadata (assoc (:metadata p) :asked-for-help new-asked-for-help)]
    (assoc p :metadata new-metadata)))


(defn mark-complete
  "Marks a project as complete"
  [p]
  (let [new-completion-info (assoc (:completion-info p) :is-complete true)]
    (assoc p :completion-info new-completion-info)))

(defn mark-incomplete
  "Marks a project as incomplete"
  [p]
  (let [new-completion-info (assoc (:completion-info p) :is-complete false)]
    (assoc p :completion-info new-completion-info)))
