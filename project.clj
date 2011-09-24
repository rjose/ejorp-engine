(defproject ejorp "0.0.1-SNAPSHOT"
            :description "Backend for ejorp"
            :dependencies [[org.clojure/clojure "1.3.0"]
                           [org.clojure/clojure-contrib "1.2.0"]
                           [joda-time "1.6"]]
            :dev-dependencies [[lein-marginalia "0.6.1"]
                               [cake-marginalia "0.6.0"]]
            :tasks [cake-marginalia.tasks])
