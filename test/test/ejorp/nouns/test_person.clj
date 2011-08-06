(ns test.ejorp.nouns.test-person
  (:use clojure.test)
  (:use ejorp.nouns.person)
  (:import ejorp.nouns.person.Person))

(def user (Person. 101 "Rino Jose"))

(deftest person-roles
  (testing "Role management"
           (let [role1 "Role1"
                 role2 "Role2"
                 user-1 (add-roles user role1 role2)
                 user-2 (remove-role user-1 role1)]
             (is (= #{role1 role2} (:roles user-1)))
             (is (= #{role2} (:roles user-2))))))
