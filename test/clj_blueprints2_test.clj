(ns clj-blueprints2-test
  (:use clj-blueprints2
        midje.sweet)
  (:import (com.tinkerpop.blueprints.impls.tg TinkerGraphFactory))
  )

(let [g (TinkerGraphFactory/createTinkerGraph)
      vs (.getVertices g)
      first-vertice (first vs)
      es (.getEdges g)
      first-edge (first es)
      
      new-node (doto (.addVertex g nil)
                 (.setProperty "one" "foo")
                 (.setProperty "two" ":bar")
                 (.setProperty "three" 42))

      new-edge (doto (.addEdge g nil first-vertice new-node "something")
                 (.setProperty "four" "quux")
                 (.setProperty "five" ":zed")
                 (.setProperty "six" 55))
      ]
  
  
  (facts "about get-id"
         (get-id first-vertice) => "3"
         (get-id first-edge) => "10"
         )

  (facts "about pget"
         (pget new-node :one) => "foo"
         (pget new-node :two) => :bar
         (pget new-node :three) => 42
         (pget new-node :four) => nil

         (pget new-edge :four) => "quux"
         (pget new-edge :five) => :zed
         (pget new-edge :six) => 55
         (pget new-edge :seven) => nil
         )

  (facts "about passoc!"
         (let [n (.addVertex g nil)]
           (passoc! n :foo "bar")
           (.getProperty n "foo")) => "bar"

         (let [n (.addVertex g nil)]
           (passoc! n :foo :bar)
           (.getProperty n "foo")) => ":bar"

         (let [n (.addVertex g nil)]
           (passoc! n :foo 42)
           (.getProperty n "foo")) => 42


         (let [n (.addVertex g nil)]
           (passoc! n :foo "bar" :bar "ofo")
           (map #(.getProperty n %) ["bar" "foo"])) => ["ofo" "bar"]

         (let [n (.addVertex g nil)]
           (passoc! n :foo :bar :bar :ofo)
           (map #(.getProperty n %) ["bar" "foo"])) => [":ofo" ":bar"]

         (let [n (.addVertex g nil)]
           (passoc! n :foo 42 :bar 55)
           (map #(.getProperty n %) ["bar" "foo"])) => [55 42]


           
         (let [n (.addEdge g nil first-vertice new-node "new")]
           (passoc! n :foo "bar")
           (.getProperty n "foo")) => "bar"

         (let [n (.addEdge g nil first-vertice new-node "new")]
           (passoc! n :foo :bar)
           (.getProperty n "foo")) => ":bar"

         (let [n (.addEdge g nil first-vertice new-node "new")]
           (passoc! n :foo 42)
           (.getProperty n "foo")) => 42


         (let [n (.addEdge g nil first-vertice new-node "new")]
           (passoc! n :foo "bar" :bar "ofo")
           (map #(.getProperty n %) ["bar" "foo"])) => ["ofo" "bar"]

         (let [n (.addEdge g nil first-vertice new-node "new")]
           (passoc! n :foo :bar :bar :ofo)
           (map #(.getProperty n %) ["bar" "foo"])) => [":ofo" ":bar"]

         (let [n (.addEdge g nil first-vertice new-node "new")]
           (passoc! n :foo 42 :bar 55)
           (map #(.getProperty n %) ["bar" "foo"])) => [55 42]

           )

  (future-facts "about pdissoc!")
  (future-facts "about pkeys")
  (future-facts "about pvals")
  (future-facts "about as-map")
  (future-facts "about with-tx")
  )


