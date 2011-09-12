(ns test.ejorp.scenarios.happy-path
  (:use clojure.test)
  (:use [ejorp.protocols.workable :as workable])  
  (:use [ejorp.nouns.project :as project])
  (:import ejorp.nouns.project.Project)
  (:use ejorp.util.date)
  (:require [ejorp.protocols.traj :as traj])
)

(def jul15 (str-to-date "2011-07-15"))
(def jul31 (str-to-date "2011-07-31"))
(def oct30 (str-to-date "2011-10-30"))
(def nov30 (str-to-date "2011-11-30"))

;; ## Scenario: New user, no team
;; * Logs in
;; Here, we'd look up the user in our database based on some auth info and
;; store some token as a cookie. This token would be used to look up user info
;; on each request. For now, we'll simulate a login.
(def current-user {:id 100, :name "Rino Jose", :pictureUrl "http://somepic.png"})

;; * Creates a project by specifying a name and planned start/end dates
(def jupiter (Project. 1001 "Jupiter" {} {}))
(def jupiter (workable/set-dates jupiter :planned [jul31 oct30]))

;; * Creates two more projects like this
(def neptune (Project. 1002 "Neptune" {} {}))
(def neptune (workable/set-dates neptune :planned [jul15 oct30]))

(def saturn (Project. 1002 "Saturn" {} {}))
(def saturn (workable/set-dates saturn :planned [oct30 nov30]))

;; * Updates project by specifying required staff
(def named-traj-f1 (traj/make-uniform-named-traj-f {"SW" 2, "QA" 1} [jul31 oct30]))
(def jupiter (workable/add-traj-f jupiter :planned-by-role named-traj-f1))

(def named-traj-f2 (traj/make-uniform-named-traj-f {"SW" 4, "QA" 1} [jul15 oct30]))
(def neptune (workable/add-traj-f neptune :planned-by-role named-traj-f2))

(def named-traj-f3 (traj/make-uniform-named-traj-f {"SW" 3, "QA" 2} [oct30 nov30]))
(def saturn (workable/add-traj-f saturn :planned-by-role named-traj-f3))

;; * Views required effort by staff or by project
(def ranges-1 [[jul15 jul31] [oct30 nov30]])
((workable/total-loading-by-role :planned-by-role [jupiter neptune saturn]) ranges-1)
((workable/loading-by-workable {"Jupiter" jupiter, "Neptune" neptune, "Saturn" saturn} :planned-by-role) ranges-1)

;; ## Scenario: Existing user, invites team member
;; ## Scenario: Invited user logs in
;; ## Scenario: Two users update a project at the same time
;; ## Scneario: User experiments with different project times
;; ## Scenario: User applies experiment
;; ## Scenario: Someone assigns a user to a project
;; ## Scenario: Someone assigns a user to two different projects

;; ## Fixtures


;;; The first thing we'll do is define a sample workable that we can test with.
;;; This doesn't do anything fancy.
;(defrecord SampleWorkable [name date-map named-traj-f])
;(extend-type SampleWorkable
;  workable/Workable
;  (date-map [w] (:date-map w))
;  (named-traj-f [w] (:named-traj-f w)))
;
;;; These are some common dates we use throughout
;(def jul30 (str-to-date "2011-07-30"))
;(def aug1 (str-to-date "2011-08-01"))
;(def aug5 (str-to-date "2011-08-05"))
;(def aug10 (str-to-date "2011-08-10"))
;(def aug12 (str-to-date "2011-08-12"))
;(def aug30 (str-to-date "2011-08-30"))
;
;(def date-ranges1 (partition 2 1 (map str-to-date [jul30 aug5 aug10 aug12])))
;
;;; Here are some uniform named-traj-f's that we'll need.
;(def named-traj-f1 (traj/make-uniform-named-traj-f {"SW" 2, "QA" 1} [jul30 aug30]))
;(def named-traj-f2 (traj/make-uniform-named-traj-f {"PM" 2} [jul30 aug30]))
;(def named-traj-f3 (traj/make-uniform-named-traj-f {"TechPub" 1} [jul30 aug30]))
;
;;; Here, we define a sample workable that we'll use for testing
;(def w1 (-> 
;          (SampleWorkable. "Jupiter" {} {})
;          (workable/set-dates :planned [jul30 aug30])
;          (workable/set-named-traj-f :planned-by-role named-traj-f1)))
;
;
;;; ## Tests
;;; This checks that we can get the duration of a workable. 
;; TODO: We should update this function so it takes a key
;(deftest test-duration
;  (is (= 31 (workable/duration w1))))
;
;;; This tests that we can get a traj-fn for a given key. This is an
;;; interesting function because it's creating the traj-fn lazily based on
;;; the named-traj-f for a workable.
;(deftest test-get-planned-trajectory
;  (let [role-traj-fn (workable/traj-fn w1 :planned-by-role)]
;    (is (= {"SW" [2], "QA" [1]} (role-traj-fn [[jul30 aug30]])))))
;
;;; This tests that we can add a traj-f to an existing named-traj-f
;(deftest test-add-traj-f
;  (let [new-w1 (workable/add-traj-f w1 :planned-by-role named-traj-f2)
;        new-traj-fn (workable/traj-fn new-w1 :planned-by-role)]
;    (is (= {"SW" [2], "QA" [1], "PM" [2]} (new-traj-fn [[jul30 aug30]])))))
;
;;; This tests that we can remove a traj-f from a named-traj-f
;(deftest test-remove-traj-f
;  (let [new-w1 (workable/remove-traj-f w1 :planned-by-role ["SW"])
;        new-traj-fn (workable/traj-fn new-w1 :planned-by-role)]
;    (is (= {"QA" [1]} (new-traj-fn [[jul30 aug30]])))))
;
;;; This tests that we can get the names of a named-traj-f for a workable.
;(deftest test-traj-names
;  (let [new-w1 (workable/remove-traj-f w1 :planned-by-role ["SW"])]
;    (is (= ["QA"] (workable/traj-names new-w1 :planned-by-role)))))
;
;
;(deftest test-shift-workable
;  (let [num-days 7
;        orig-dates (workable/get-dates w1 :planned)
;        shifted-proj (workable/shift-workable w1 :planned num-days)
;        new-dates (workable/get-dates shifted-proj :planned)]
;    (is (= (first new-dates) (.plusDays (first orig-dates) num-days)))
;    (is (= (last new-dates) (.plusDays (last orig-dates) num-days)))))
