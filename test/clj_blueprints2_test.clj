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

  (facts "about pdissoc!"
         (let [n (doto (.addVertex g nil)
                   (.setProperty "one" "foo"))]
           (pdissoc! n :one)
           (.getProperty n "one")) => nil

         (let [n (doto (.addVertex g nil)
                   (.setProperty "one" "foo")
                   (.setProperty "two" "bar")
                   )]
           (pdissoc! n :one :two)
           (map #(.getProperty n %) ["one" "two"])) => [nil nil]

           
         (let [n (doto (.addEdge g nil first-vertice new-node "new")
                   (.setProperty "one" "foo"))]
           (pdissoc! n :one)
           (.getProperty n "one")) => nil

         (let [n (doto (.addEdge g nil first-vertice new-node "new")
                   (.setProperty "one" "foo")
                   (.setProperty "two" "bar")
                   )]
           (pdissoc! n :one :two)
           (map #(.getProperty n %) ["one" "two"])) => [nil nil]
           
         )

  (facts "about pkeys"
         (pkeys first-vertice) => (just #{:name :lang})
         (pkeys new-node) => (just #{:one :two :three})

         (pkeys first-edge) => (just #{:weight})
         (pkeys new-edge) => (just #{:four :five :six})
         )

  (facts "about pvals"
         (pvals first-vertice) => (just #{"lop" "java"})
         (pvals new-node) => (just #{:bar "foo" 42})

         (pvals first-edge) => (just #{1.0})
         (pvals new-edge) => (just #{:zed "quux" 55})
         )

  (facts "about as-map"
         (as-map first-vertice) => {:name "lop", :lang "java"}
         (as-map new-node) => {:one "foo", :three 42, :two :bar}

         (as-map first-edge) => {:weight 1.0}
         (as-map new-edge) => {:six 55, :five :zed, :four "quux"}
         )

  (future-facts "about with-tx")
  (future-facts "about with-db")
  (future-facts "about set-db!")
  (future-facts "about shutdown!")
  (future-facts "about clear!")
  (future-facts "about get-vertices")
  (future-facts "about get-edges")
  (future-facts "about load-vertex")
  (future-facts "about load-edge")
  (future-facts "about vertex")
  (future-facts "about link!")
  (future-facts "about remove!")
  (future-facts "about get-edges")
  (future-facts "about get-vertex")
  (future-facts "about get-label")
  (future-facts "about get-ends")
  (future-facts "about get-link")
  (future-facts "about linked?")
  (future-facts "about unlink!")
  (future-facts "about create-automatic-index!")
  (future-facts "about create-manual-index!")
  (future-facts "about get-index")
  (future-facts "about get-indices")
  (future-facts "about drop-index!")
  (future-facts "about index-class")
  (future-facts "about index-name")
  (future-facts "about index-type")
  (future-facts "about iget")
  (future-facts "about iput")
  (future-facts "about iremove")
  (future-facts "about as-read-only")
  (future-facts "about migrate-graph!")
  (future-facts "about read-graph-ml!")
  (future-facts "about write-graph-ml!")
  (future-facts "about read-graph-json!")
  (future-facts "about write-graph-json!")
  (future-facts "about graph-listener")
  (future-facts "about event-graph")
  (future-facts "about add-listener")
  (future-facts "about get-raw-graph")
  (future-facts "about tinker-graph")
  
  )


