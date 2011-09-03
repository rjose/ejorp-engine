(ns test.ejorp.reports.test-loading
  (:use clojure.test)
  (:use clojure.contrib.generic.math-functions)
  (:use ejorp.reports.loading)  
  (:use fixtures.general))

;(deftest test-resource-availability
;  (let [avail (resource-availability sw-team [jupiter-loading])
;        node-avail (avail "Node Engineer")
;        warblade-avail (avail "Warblade Knight")
;        ]
;    (is (approx= 1.49 (first node-avail) 0.01))
;    (is (approx= 1.0 (first warblade-avail) 0.01))))
