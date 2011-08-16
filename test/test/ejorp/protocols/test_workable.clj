(ns test.ejorp.protocols.test-workable
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use ejorp.protocols.workable)  
  (:use ejorp.util.date)
  (:require [ejorp.protocols.load-traj :as load-traj]))

;; Create a sample workable
(defrecord SampleWorkable [name])

(defn set-planned-start-end 
  [w start end]
  (let [planned-dates (assoc (:planned-dates w) :start start :end end)]
    (assoc w :planned-dates planned-dates)))

; TODO: Add this to the load-traj module
(defn make-uniform-load-traj
  "Constructs a load-traj function with uniform density over a time period"
  [scale [start-date end-date]]
  (let [density-f (load-traj/scale-density-fn scale load-traj/uniform-density)
        traj-f (partial load-traj/load-traj start-date end-date density-f)]
    traj-f))

(def sw-load-traj (make-uniform-load-traj 2.0 (map str-to-date ["2011-07-30" "2011-08-30"])))
(def qa-load-traj (make-uniform-load-traj 1.0 (map str-to-date ["2011-07-30" "2011-08-30"])))



; TODO: Add to load-traj module
; This is a convenience function for initial planning
(defn make-composite-uniform-load-traj
  "Constructs a map of roles to uniform load trajectories"
  [scale-map date-range]
  (into {} (map (fn [[role scale]] [role (make-uniform-load-traj scale date-range)]) scale-map)))

(def traj-map1 (make-composite-uniform-load-traj {"SW" 2.0, "QA" 1.0} (map str-to-date ["2011-07-30" "2011-08-30"])))

(defn traj-seq
  "Computes a seq of load trajectories given a load-traj function and a seq of date ranges"
  [load-traj date-ranges]
  (map load-traj date-ranges))

; TODO: Add to load-traj module and think of a better name
(defn all-load-traj
  "Builds a function that returns the loading trajectory for all roles"
  [load-map]
  (fn [date-ranges]
    (into {} (map (fn [[role traj-f]] [role (traj-seq traj-f date-ranges)]) load-map))))  

(defn shift-date-range
  [date-range num-days]
  (map #(.minusDays % num-days) date-range))

(defn shift-date-ranges
  [date-ranges num-days]
  (map #(shift-date-range % num-days) date-ranges))

(defn shift-date-ranges-f
  "Creates a function that shifts date ranges by num-days"
  [num-days]
  (fn [date-ranges] (shift-date-ranges date-ranges num-days)))

(def workable-traj-f1 (all-load-traj traj-map1))
(def date-ranges1 (partition 2 1 (map str-to-date ["2011-07-30" "2011-08-05" "2011-08-10" "2011-08-12"])))

(defn shift
  "Returns a new loading trajectory shifted in time by num-days"
  [workable-traj-f num-days]
  (comp workable-traj-f (shift-date-ranges-f num-days)))
  

(extend-type SampleWorkable
  Workable
                              
  (start-date
    [w]
    (:start (:planned-dates w)))

  (end-date
    [w]
    (:end (:planned-dates w))))

(def sample-workable (-> (SampleWorkable. "Sample")
                       (set-planned-start-end (str-to-date "2011-07-30") (str-to-date "2011-08-30"))))

(deftest test-duration
  (let [one-day-workable (set-planned-start-end sample-workable (str-to-date "2011-07-30") (str-to-date "2011-07-31"))
        zero-day-workable (set-planned-start-end sample-workable (str-to-date "2011-07-30") (str-to-date "2011-07-30"))
        negative-day-workable (set-planned-start-end sample-workable (str-to-date "2011-07-30") (str-to-date "2011-07-20"))]
    (is (= 1 (duration one-day-workable)))
    (is (= 0 (duration zero-day-workable)))
    (is (= 10 (duration negative-day-workable)))))

(deftest test-fraction-of
  (let [w (set-planned-start-end sample-workable (str-to-date "2011-07-20") (str-to-date "2011-07-30"))]
    (is (approx= 0.0 (fraction-of w (str-to-date "2011-07-20")) 0.01))
    (is (approx= 1.0 (fraction-of w (str-to-date "2011-07-30")) 0.01))
    (is (approx= 0.5 (fraction-of w (str-to-date "2011-07-25")) 0.01))
    (is (approx= 0.0 (fraction-of w (str-to-date "2011-07-15")) 0.01))
    (is (approx= 1.0 (fraction-of w (str-to-date "2011-08-15")) 0.01))
    ))

(deftest test-clamp-date
  (let [start-date (str-to-date "2011-07-20")
        end-date (str-to-date "2011-07-30")
        w (set-planned-start-end sample-workable start-date end-date)        
        mid-date (str-to-date "2011-07-22")
        early-date (str-to-date "2011-07-01")
        late-date (str-to-date "2011-09-01")
        ]
    (is (= mid-date (clamp-date w mid-date)))
    (is (= start-date (clamp-date w early-date)))
    (is (= end-date (clamp-date w late-date)))))


;(deftest test-loading-traj
;  (let [date-range1 (map str-to-date ["2011-07-30" "2011-08-15"])
;        traj (load-traj/load-traj sample-workable [date-range1])
;        node-eng-load (traj "Node Engineer")]
;    (is (approx= 0.97 (nth node-eng-load 1) 0.01))))

; TODO: Workables should have loading trajectories. We should test this 
