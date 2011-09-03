(ns test.ejorp.protocols.test-workable
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use [ejorp.protocols.workable :as workable])  
  (:use ejorp.util.date)
  (:require [ejorp.protocols.traj :as traj])
)

;; ## Fixtures
;; The first thing we'll do is define a sample workable that we can test with.
;; This doesn't do anything fancy.
(defrecord SampleWorkable [name date-map named-traj-f])
(extend-type SampleWorkable
  workable/Workable
  (date-map [w] (:date-map w))
  (named-traj-f [w] (:named-traj-f w)))

;; These are some common dates we use throughout
(def jul30 (str-to-date "2011-07-30"))
(def aug1 (str-to-date "2011-08-01"))
(def aug5 (str-to-date "2011-08-05"))
(def aug10 (str-to-date "2011-08-10"))
(def aug12 (str-to-date "2011-08-12"))
(def aug30 (str-to-date "2011-08-30"))

(def date-ranges1 (partition 2 1 (map str-to-date [jul30 aug5 aug10 aug12])))

;; Here are some uniform named-traj-f's that we'll need.
(def named-traj-f1 (traj/make-uniform-named-traj-f {"SW" 2, "QA" 1} [jul30 aug30]))
(def named-traj-f2 (traj/make-uniform-named-traj-f {"PM" 2} [jul30 aug30]))
(def named-traj-f3 (traj/make-uniform-named-traj-f {"TechPub" 1} [jul30 aug30]))

;; Here, we define a sample workable that we'll use for testing
(def w1 (-> 
          (SampleWorkable. "Jupiter" {} {})
          (workable/set-dates :planned [jul30 aug30])
          (workable/set-named-traj-f :planned-by-role named-traj-f1)))


;; ## Tests
;; This checks that we can get the duration of a workable. 
; TODO: We should update this function so it takes a key
(deftest test-duration
  (is (= 31 (workable/duration w1))))

;; This tests that we can get a named-traj-fn for a given key. This is an
;; interesting function because it's creating the named-traj-fn lazily based on
;; the named-traj-f for a workable.
(deftest test-get-planned-trajectory
  (let [role-traj-fn (workable/named-traj-fn w1 :planned-by-role)]
    (is (= {"SW" [2], "QA" [1]} (role-traj-fn [[jul30 aug30]])))))

;; This tests that we can add a traj-f to an existing named-traj-f
(deftest test-add-traj-f
  (let [new-w1 (workable/add-traj-f w1 :planned-by-role named-traj-f2)
        new-traj-fn (workable/named-traj-fn new-w1 :planned-by-role)]
    (is (= {"SW" [2], "QA" [1], "PM" [2]} (new-traj-fn [[jul30 aug30]])))))

;; This tests that we can remove a traj-f from a named-traj-f
(deftest test-remove-traj-f
  (let [new-w1 (workable/remove-traj-f w1 :planned-by-role ["SW"])
        new-traj-fn (workable/named-traj-fn new-w1 :planned-by-role)]
    (is (= {"QA" [1]} (new-traj-fn [[jul30 aug30]])))))

;; This tests that we can get the names of a named-traj-f for a workable.
(deftest test-traj-names
  (let [new-w1 (workable/remove-traj-f w1 :planned-by-role ["SW"])]
    (is (= ["QA"] (workable/traj-names new-w1 :planned-by-role)))))


(deftest test-shift-workable
  (let [num-days 7
        orig-dates (workable/get-dates w1 :planned)
        shifted-proj (workable/shift-workable w1 :planned num-days)
        new-dates (workable/get-dates shifted-proj :planned)]
    (is (= (first new-dates) (.plusDays (first orig-dates) num-days)))
    (is (= (last new-dates) (.plusDays (last orig-dates) num-days)))))
