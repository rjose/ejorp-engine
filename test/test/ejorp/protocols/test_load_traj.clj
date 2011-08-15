(ns test.ejorp.protocols.test-load-traj
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)  
  (:use ejorp.util.date)
  (:use ejorp.protocols.load-traj))


(deftest test-load-traj
  (let [start-date (str-to-date "2011-08-10")
        end-date (str-to-date "2011-08-16")
        density-f (scale-density-fn 2.5 uniform-density)
        traj-f (partial load-traj start-date end-date density-f)]
    (is (approx= 1.25 (traj-f [start-date (str-to-date "2011-08-13")]) 0.1))))

; TODO: Test other load-traj functions
; TODO: Add load-traj to workables
; TODO: Extract loading summations from workables and use them separately
; TODO: Rewrite project so it uses load-traj functions for each role
; TODO: Create different kinds of load functions