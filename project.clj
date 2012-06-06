(defproject clj-blueprints2 "0.0.1"
  :description "Wrapper for the TinkerPop Blueprints API for Graph DBMSs. It supports version 2.0 of the Blueprints API."
  :url "https://github.com/olabini/clj-blueprints"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [com.tinkerpop.blueprints/blueprints-core "2.0.0"]]
  :dev-dependencies [[midje "1.4.0"]]
  )
