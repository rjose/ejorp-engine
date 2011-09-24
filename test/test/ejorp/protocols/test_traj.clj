(ns test.ejorp.protocols.test-traj
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)  
  (:use ejorp.util.date)
  (:use ejorp.protocols.traj)
  (:require [ejorp.util.density-integrals :as density]))

;; ### Fixtures
;; Here are the test fixtures shared between tests. They're convenient to have here
;; for experimenting in the repl.

;; All dates use `joda-time` to do date computation. Use `str-to-date` to create them.
(def aug-5 (str-to-date "2011-08-05"))
(def aug-8 (str-to-date "2011-08-08"))
(def aug-10 (str-to-date "2011-08-10"))
(def aug-11 (str-to-date "2011-08-11"))
(def aug-12 (str-to-date "2011-08-12"))
(def aug-13 (str-to-date "2011-08-13"))
(def aug-14 (str-to-date "2011-08-14"))
(def aug-15 (str-to-date "2011-08-15"))
(def aug-16 (str-to-date "2011-08-16"))
(def aug-25 (str-to-date "2011-08-25"))

;; Here are some uniform `traj-f` functions that have uniform density functions
;; of different scales.
(def uniform-traj-1 (make-uniform-traj-f 1.0 [aug-10 aug-16]))
(def uniform-traj-3 (make-uniform-traj-f 3.0 [aug-10 aug-16]))

;; These are named trajectory functions that can be used to generate named-traj
;; sequences.
(def named-traj-f1 {"SW" uniform-traj-3, "QA" uniform-traj-1})
(def named-traj-f2 {"SW" uniform-traj-1, "QA" uniform-traj-1})
(def traj-fn1 (make-traj-fn named-traj-f1))
(def traj-fn2 (make-traj-fn named-traj-f2))
(def named-traj1 {"SW" [1.0 2.0 3.0 1.0], "QA" [0.0 0.0 0.5 0.5]})
(def named-traj2 {"SW" [1.0 1.0 1.0 1.0], "QA" [0.5 0.5 0.5 0.5]})


;; This is effort data that can be used for constructing a traj-f
(def effort-data1 {:start-date aug-8, :values [1 2 3 4 5 5 4 3 2 1]})
(def effort-data2 {:start-date aug-16, :values [3 2]})

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
        load-traj-f (density-integral-to-traj-f [aug-10 aug-16] density-f)]
    (is (approx= 1.25 (first (load-traj-f [[aug-10 aug-13]])) 0.1))))

(deftest test-make-uniform-traj-f
  (let [uniform-traj-f1 (make-uniform-traj-f 6 [aug-10 aug-16])]
    (is (= [0] (uniform-traj-f1 [[aug-10 aug-10]])))
    (is (= [1] (uniform-traj-f1 [[aug-10 aug-11]])))
    (is (= [2] (uniform-traj-f1 [[aug-10 aug-12]])))
    (is (= [1] (uniform-traj-f1 [[aug-12 aug-13]])))
    (is (= [3] (uniform-traj-f1 [[aug-10 aug-13]])))
    (is (= [6] (uniform-traj-f1 [[aug-10 aug-16]])))
    (is (= [6.0] (uniform-traj-f1 [[aug-5 aug-25]])))
    ))

;; Building uniform-named-traj-f is the starting point for doing any
;; first cut at modeling project resource planning.
(deftest test-make-uniform-named-traj-f
  (is (approx= 3.0 (first (uniform-traj-3 [[aug-10 aug-16]])) 0.1))
  (is (approx= 1.5 (first (uniform-traj-3 [[aug-10 aug-13]])) 0.1))
  (is (approx= 0.0 (first (uniform-traj-3 [[aug-5 aug-10]])) 0.1))
  (is (= 0.0 (first (uniform-traj-3 [[aug-10 aug-10]]))))
  (is (approx= 0.0 (first (uniform-traj-3 [[aug-16 aug-25]])) 0.1)))
  
;; The `traj-fn` will be the workhouse of any project loading computations.
(deftest test-make-traj-fn
  (let [results (traj-fn1 [[aug-10 aug-13]])]
    (is (approx= 1.5 (first (results "SW")) 0.1))
    (is (approx= 0.5 (first (results "QA")) 0.1))))

;; Shifting a date range by x days returns a new date range shifted x days into the past.
(deftest test-shift-date-range
  (is (= [aug-5 aug-10] (shift-date-range [aug-10 aug-15] 5))))

;; This tests shifting date-ranges in time.
(deftest test-shift-date-ranges
  (is (= [[aug-5 aug-10] [aug-8 aug-11]] (shift-date-ranges [[aug-10 aug-15] [aug-13 aug-16]] 5))))

;; This tests the shifting of a traj-f in time. Note that while the original intent
;; of shift-traj-f was to literally shift traj-f functions in time, it can
;; correctly shift any function of date-ranges in time. In this case, we're shifting
;; a traj-fn in time.
(deftest test-shift-traj-f
  (let [ranges1 [[aug-10 aug-16]]
        named-traj-shifted-0 ((shift-traj-f traj-fn1 0) ranges1)
        named-traj-shifted-3 ((shift-traj-f traj-fn1 3) ranges1)
        named-traj-shifted-6 ((shift-traj-f traj-fn1 6) ranges1)]
    (is (= {"SW" [3.0], "QA" [1.0]} named-traj-shifted-0))
    (is (= {"SW" [1.5], "QA" [0.5]} named-traj-shifted-3))
    (is (= {"SW" [0.0], "QA" [0.0]} named-traj-shifted-6))))

;; This tests the basics of summing a set of trajs.
(deftest test-sum-traj
  (let [traj1 [1.0 2.0 3.0 4.0]
        traj2 [1.0 1.0 1.0 1.0]
        traj3 [2.0 4.0 6.0 0.0]]
    (is (= [2.0 3.0 4.0 5.0] (sum-traj traj1 traj2)))
    (is (= [4.0 7.0 10.0 5.0] (sum-traj traj1 traj2 traj3)))
    (is (= traj1 (sum-traj traj1)))
    (is (= [] (sum-traj)))))

;; This tests the various cases for summing named-trajs.
(deftest test-sum-named-traj
  (let [named-traj3 {"PM" [0.2 0.2 0.2 0.2]}
        named-traj4 {}]
    (is (= {"SW" [2.0 3.0 4.0 2.0], "QA" [0.5 0.5 1.0 1.0]} (sum-named-traj named-traj1 named-traj2)))
    (is (= {"SW" [1.0 2.0 3.0 1.0], "QA" [0.0 0.0 0.5 0.5], "PM" [0.2 0.2 0.2 0.2]} 
           (sum-named-traj named-traj1 named-traj3)))
    (is (= {} (sum-named-traj)))))

;; This tests that we can make a traj-fn. This is a component used in the
;; `effort-data-to-traj-f` function.
(deftest test-make-traj-f-element
  (let [traj-f-element (make-traj-f-element aug-8 [1 2 3 4 5 5 4 3 2 1 ])]
    (is (= 0 (traj-f-element [aug-5 aug-8])))
    (is (= 3 (traj-f-element [aug-8 aug-10])))
    (is (= 24 (traj-f-element [aug-10 aug-16]))) 
    (is (= 0 (traj-f-element [aug-10 aug-10])))))

;; This demonstrates how to create a traj-f from data.
(deftest test-effort-data-to-traj-f
  (let [my-traj-f (effort-data-to-traj-f effort-data1)]
    (is (= [0 3 24] (my-traj-f [[aug-5 aug-8] [aug-8 aug-10] [aug-10 aug-16]])))))

;; This exercises the named version of creating a traj-f from data
(deftest test-effort-data-to-named-traj-f
  (let [my-named-traj-f (effort-data-to-named-traj-f {"SW" effort-data1, "QA" effort-data2})]
    (is (= {"SW" [24], "QA" [0]} (my-named-traj-f [[aug-10 aug-16]])))))

;; This tests that we can sum two traj-fn's together.
(deftest test-sum-traj-fns
  (let [sum-fn (sum-traj-fns [traj-fn1 traj-fn2])]
    (is (= {"SW" [4.0], "QA" [2.0]} (sum-fn [[aug-10 aug-16]])))))

;; This tests that we can squash a traj-fn.
(deftest test-squash-traj-fn
  (let [squashed-fn (squash-traj-fn traj-fn1)]
    (is (= [4.0] (squashed-fn [[aug-10 aug-16]])))))
