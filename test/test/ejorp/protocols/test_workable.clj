(ns test.ejorp.protocols.test-workable
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use ejorp.protocols.workable)  
  (:use ejorp.util.date)
  (:require [ejorp.protocols.traj :as traj]))

;; Create a sample workable
(defrecord SampleWorkable [name])

(defn set-planned-start-end 
  [w start end]
  (let [planned-dates (assoc (:planned-dates w) :start start :end end)]
    (assoc w :planned-dates planned-dates)))

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

(def sw-load-traj (traj/make-uniform-traj-f 2.0 (map str-to-date ["2011-07-30" "2011-08-30"])))
(def qa-load-traj (traj/make-uniform-traj-f 1.0 (map str-to-date ["2011-07-30" "2011-08-30"])))
(def traj-map1 (traj/make-uniform-named-traj-f {"SW" 2.0, "QA" 1.0} (map str-to-date ["2011-07-30" "2011-08-30"])))

(def workable-traj-f1 (traj/make-named-traj-fn traj-map1))
(def date-ranges1 (partition 2 1 (map str-to-date ["2011-07-30" "2011-08-05" "2011-08-10" "2011-08-12"])))

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
;        traj (traj/load-traj sample-workable [date-range1])
;        node-eng-load (traj "Node Engineer")]
;    (is (approx= 0.97 (nth node-eng-load 1) 0.01))))

; TODO: Workables should have loading trajectories. We should test this 
