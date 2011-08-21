(ns test.ejorp.protocols.test-load-traj
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)  
  (:use ejorp.util.date)
  (:use ejorp.protocols.load-traj)
  (:require [ejorp.util.density-integrals :as density]))

;; ## Test Data

;; #### Dates
(def aug-5 (str-to-date "2011-08-05"))
(def aug-10 (str-to-date "2011-08-10"))
(def aug-13 (str-to-date "2011-08-13"))
(def aug-16 (str-to-date "2011-08-16"))
(def aug-25 (str-to-date "2011-08-25"))

;; #### Raw Trajectories
(def uniform-traj-1 (make-uniform-load-traj 1.0 [aug-10 aug-16]))
(def uniform-traj-3 (make-uniform-load-traj 3.0 [aug-10 aug-16]))

;; #### Loading Trajectories
(def rolemap {"SW" uniform-traj-3, "QA" uniform-traj-1})
(def traj-f (build-load-traj-f rolemap))


;; ## Utility Functions
(deftest test-clamp-date
  (is (= aug-13 (clamp-date [aug-13 aug-16] aug-10)))
  (is (= aug-13 (clamp-date [aug-10 aug-13] aug-16)))
  (is (= aug-13 (clamp-date [aug-10 aug-16] aug-13))))

(deftest test-fraction-of
  (is (approx= 0.0 (fraction-of [aug-10 aug-16] aug-10) 0.1))
  (is (approx= 1.0 (fraction-of [aug-10 aug-16] aug-16) 0.1))
  (is (approx= 0.5 (fraction-of [aug-10 aug-16] aug-13) 0.1))
  (is (approx= 0.0 (fraction-of [aug-10 aug-16] aug-5) 0.1))
  (is (approx= 1.0 (fraction-of [aug-10 aug-16] aug-25) 0.1)))

;; ## Load Trajectory Functions

;; We're basically testing the construction of load-traj function.
(deftest test-load-traj
  (let [density-f (density/scale-density-integral 2.5 density/uniform-density-integral)
        load-traj-f (partial load-traj aug-10 aug-16 density-f)]
    (is (approx= 1.25 (load-traj-f [aug-10 aug-13]) 0.1))))

(deftest test-make-uniform-load-traj
  (is (approx= 3.0 (uniform-traj-3 [aug-10 aug-16]) 0.1))
  (is (approx= 1.5 (uniform-traj-3 [aug-10 aug-13]) 0.1))
  (is (approx= 0.0 (uniform-traj-3 [aug-5 aug-10]) 0.1))
  (is (approx= 0.0 (uniform-traj-3 [aug-16 aug-25]) 0.1)))
  
(deftest test-build-load-traj-f
  (let [results (traj-f [[aug-10 aug-13]])]
    (is (approx= 1.5 (first (results "SW")) 0.1))
    (is (approx= 0.5 (first (results "QA")) 0.1))))




; TODO: Test other load-traj functions
; TODO: Add load-traj to workables
; TODO: Extract loading summations from workables and use them separately
; TODO: Rewrite project so it uses load-traj functions for each role
; TODO: Create different kinds of load functions