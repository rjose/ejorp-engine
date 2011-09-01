(ns test.ejorp.protocols.test-workable
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use [ejorp.protocols.workable :as workable])  
  (:use ejorp.util.date)
  (:require [ejorp.protocols.traj :as traj])
)

;; ## Fixtures
(defrecord SampleWorkable [name date-map-ref named-traj-map-ref])

(extend-type SampleWorkable
  workable/Workable
  (date-map-ref [w] (:date-map-ref w))
  (named-traj-map-ref [w] (:named-traj-map-ref w)))

(def w1 (SampleWorkable. "Jupiter" (ref {}) (ref {})))
(def jul30 (str-to-date "2011-07-30"))
(def aug1 (str-to-date "2011-08-01"))
(def aug30 (str-to-date "2011-08-30"))

(def traj-map1 (traj/make-uniform-named-traj-f {"SW" 2, "QA" 1} [jul30 aug30]))
(def workable-traj-fn1 (traj/make-named-traj-fn traj-map1))

(dosync (workable/set-dates w1 :planned [jul30 aug30]))
(dosync (workable/set-named-traj-fn w1 :planned-by-role workable-traj-fn1))

(def date-ranges1 (partition 2 1 (map str-to-date ["2011-07-30" "2011-08-05" "2011-08-10" "2011-08-12"])))

;; ## Tests
(deftest test-duration
  (is (= 31 (workable/duration w1))))

(deftest test-get-planned-trajectory
  (let [role-traj-fn (workable/named-traj-fn w1 :planned-by-role)]
    (is (= {"SW" [2], "QA" [1]} (role-traj-fn [[jul30 aug30]])))))

