(ns test.ejorp.protocols.test-load-traj
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)  
  (:use ejorp.util.date)
  (:use ejorp.protocols.load-traj)
  (:require [ejorp.util.density-integrals :as density]))


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

;; TODO: Test make-uniform-load-traj
;; TODO: Test build-load-traj-f

;; We're basically testing the construction of load-traj function.
(deftest test-load-traj
  (let [start-date (str-to-date "2011-08-10")
        end-date (str-to-date "2011-08-16")
        density-f (density/scale-density-integral 2.5 density/uniform-density-integral)
        traj-f (partial load-traj start-date end-date density-f)]
    (is (approx= 1.25 (traj-f [start-date (str-to-date "2011-08-13")]) 0.1))))


; TODO: Test other load-traj functions
; TODO: Add load-traj to workables
; TODO: Extract loading summations from workables and use them separately
; TODO: Rewrite project so it uses load-traj functions for each role
; TODO: Create different kinds of load functions