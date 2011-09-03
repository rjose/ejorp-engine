(ns test.ejorp.protocols.test-workable
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use [ejorp.protocols.workable :as workable])  
  (:use ejorp.util.date)
  (:require [ejorp.protocols.traj :as traj])
)

;; ## Fixtures
(defrecord SampleWorkable [name date-map named-traj-f])

(extend-type SampleWorkable
  workable/Workable
  (date-map [w] (:date-map w))
  (named-traj-f [w] (:named-traj-f w)))

(def jul30 (str-to-date "2011-07-30"))
(def aug1 (str-to-date "2011-08-01"))
(def aug30 (str-to-date "2011-08-30"))

(def traj-map1 (traj/make-uniform-named-traj-f {"SW" 2, "QA" 1} [jul30 aug30]))
;(def workable-traj-fn1 (traj/make-named-traj-fn traj-map1))
(def traj-map2 (traj/make-uniform-named-traj-f {"PM" 2} [jul30 aug30]))
(def traj-map3 (traj/make-uniform-named-traj-f {"TechPub" 1} [jul30 aug30]))

(def workable-traj-fn1 (traj/make-named-traj-fn traj-map1))

(def w1 (-> 
          (SampleWorkable. "Jupiter" {} {})
          (workable/set-dates :planned [jul30 aug30])
          (workable/set-named-traj-f :planned-by-role traj-map1)))

(def date-ranges1 (partition 2 1 
                             (map str-to-date 
                                  ["2011-07-30" "2011-08-05" "2011-08-10" "2011-08-12"])))

;; ## Tests
(deftest test-duration
  (is (= 31 (workable/duration w1))))

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

(deftest test-traj-names
  (let [new-w1 (workable/remove-traj-f w1 :planned-by-role ["SW"])]
    (is (= ["QA"] (workable/traj-names new-w1 :planned-by-role)))))
