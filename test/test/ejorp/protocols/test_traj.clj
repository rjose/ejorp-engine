(ns test.ejorp.protocols.test-traj
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)  
  (:use ejorp.util.date)
  (:use ejorp.protocols.traj)
  (:require [ejorp.util.density-integrals :as density]))

;; ### Fixtures
;; Here are the test fixtures shared between tests. They're convenient to have here
;; for experimenting in the repl.

;; #### Dates
;; All dates use `joda-time` to do date computation. Use `str-to-date` to create them.
(def aug-5 (str-to-date "2011-08-05"))
(def aug-10 (str-to-date "2011-08-10"))
(def aug-13 (str-to-date "2011-08-13"))
(def aug-16 (str-to-date "2011-08-16"))
(def aug-25 (str-to-date "2011-08-25"))

;; #### Trajectory Functions
;; Here are some uniform `traj-f` functions that have uniform density functions
;; of different scales.
(def uniform-traj-1 (make-uniform-traj-f 1.0 [aug-10 aug-16]))
(def uniform-traj-3 (make-uniform-traj-f 3.0 [aug-10 aug-16]))

;; These are named trajectory functions that can be used to generate named-traj
;; sequences.
(def named-traj-f {"SW" uniform-traj-3, "QA" uniform-traj-1})
(def named-traj-fn (make-named-traj-fn named-traj-f))

;; ### Tests
;; This first set of tests exercises the `clamp-date` functions.
(deftest test-clamp-date
  (is (= aug-13 (clamp-date [aug-13 aug-16] aug-10)))
  (is (= aug-13 (clamp-date [aug-10 aug-13] aug-16)))
  (is (= aug-13 (clamp-date [aug-10 aug-16] aug-13))))

;; The `fraction-of` function is important when converting date-ranges
;; to frac-ranges.
(deftest test-fraction-of
  (is (approx= 0.0 (fraction-of [aug-10 aug-16] aug-10) 0.1))
  (is (approx= 1.0 (fraction-of [aug-10 aug-16] aug-16) 0.1))
  (is (approx= 0.5 (fraction-of [aug-10 aug-16] aug-13) 0.1))
  (is (approx= 0.0 (fraction-of [aug-10 aug-16] aug-5) 0.1))
  (is (approx= 1.0 (fraction-of [aug-10 aug-16] aug-25) 0.1)))

;; Here, we're basically testing the construction of load-traj function.
(deftest test-load-traj
  (let [density-f (density/scale-density-integral 2.5 density/uniform-density-integral)
        load-traj-f (make-traj-f [aug-10 aug-16] density-f)]
    (is (approx= 1.25 (first (load-traj-f [[aug-10 aug-13]])) 0.1))))

;; Building uniform-named-traj-f is the starting point for doing any
;; first cut at modeling project resource planning.
(deftest test-make-uniform-named-traj-f
  (is (approx= 3.0 (first (uniform-traj-3 [[aug-10 aug-16]])) 0.1))
  (is (approx= 1.5 (first (uniform-traj-3 [[aug-10 aug-13]])) 0.1))
  (is (approx= 0.0 (first (uniform-traj-3 [[aug-5 aug-10]])) 0.1))
  (is (approx= 0.0 (first (uniform-traj-3 [[aug-16 aug-25]])) 0.1)))
  
;; The `named-traj-fn` will be the workhouse of any project loading computations.
(deftest test-make-named-traj-fn
  (let [results (named-traj-fn [[aug-10 aug-13]])]
    (is (approx= 1.5 (first (results "SW")) 0.1))
    (is (approx= 0.5 (first (results "QA")) 0.1))))
