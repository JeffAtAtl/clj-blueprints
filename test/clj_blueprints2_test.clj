(ns clj-blueprints2-test
  (:use clj-blueprints2
        midje.sweet)
  (:import (com.tinkerpop.blueprints Vertex Direction)
           (com.tinkerpop.blueprints.impls.tg TinkerGraphFactory))
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

  (facts "about with-db"
    (let [g2 (TinkerGraphFactory/createTinkerGraph)]
      [(with-db g2
         *db*) *db*] => [g2 nil]
         ))

  (facts "about set-db!"
    (let [g2 (TinkerGraphFactory/createTinkerGraph)]
      [(set-db! g2) *db*] => [g2 g2]))

  (facts "about clear!"
    (let [g2 (TinkerGraphFactory/createTinkerGraph)]
      (with-db g2
        (clear!)
        (.getVertices g2)) => empty?))

  (facts "about get-vertices"
    (with-db g
      (map get-id (get-vertices))) => ["19" "11" "12" "3" "20" "2" "1" "10" "0" "7" "6" "5" "4" "9" "8"])

  (facts "about get-edges"
    (with-db g
      (map get-id (get-edges))) => ["22" "17" "18" "15" "16" "13" "14" "11" "12" "21" "10" "1" "7" "9" "8"])

  (facts "about load-vertex"
    (with-db g
      (as-map (load-vertex 12)) => {:foo 42
                                    :bar 55}))

  (facts "about load-edge"
    (with-db g
      (as-map (load-edge 17)) => {:foo :bar
                                  :bar :ofo}))

  (facts "about vertex"
    (with-db (TinkerGraphFactory/createTinkerGraph)
      (vertex) => (fn [other] (instance? Vertex other))
      (get-id (vertex 42)) => "42"
      (as-map (vertex {:foo 14 :bar 53})) => {:foo 14 :bar 53}
      (get-id (vertex 43 {})) => "43"
      (get-id (vertex 45 nil)) => "45"
      (as-map (vertex 44 {:aba 15})) => {:aba 15}))

  (facts "about link!"
    (let [g2 (TinkerGraphFactory/createTinkerGraph)
          vs (.getVertices g2)
          v1 (first vs)
          v2 (second vs)]
      (with-db g2
        (.getLabel (link! v1 :foo v2)) => "foo"
        (.getVertex (link! v1 :foo v2) Direction/IN) => v2
        (.getVertex (link! v1 :foo v2) Direction/OUT) => v1
        (as-map (link! v1 :bar {:blarg 55} v2)) => {:blarg 55}
        (get-id (link! 54465 v1 :quux {:blarg 4354} v2)) => "54465"
        )))

  (facts "about remove!"
    (let [g2 (TinkerGraphFactory/createTinkerGraph)
          v (first (.getVertices g2))
          e (first (.getEdges g2))]
      (with-db g2
        (do (remove! v)
          (.getVertex g2 (get-id v))) => nil
        (do (remove! e)
          (.getEdge g2 (get-id e))) => nil
          )))

  (facts "about get-label"
    (get-label first-edge) => "created"
    (get-label (second es)) => "knows"
    )

  (facts "about get-edges"
    (map get-id (get-edges first-vertice :in)) => ["9" "11" "12"]
    (map get-id (get-edges first-vertice :out)) => ["21" "22" "17" "18" "15" "16" "13" "14" "1"]
    (map get-id (get-edges first-vertice :both)) => ["9" "11" "12" "21" "22" "17" "18" "15" "16" "13" "14" "1"]

    (map get-id (get-edges first-vertice :in "foo")) => []
    (map get-id (get-edges first-vertice :in "created")) => ["9" "11" "12"]

    (map get-id (get-edges first-vertice :out "new")) => ["21" "22" "17" "18" "15" "16" "13" "14"]
    (map get-id (get-edges first-vertice :out "something")) => ["1"]

    (map get-id (get-edges first-vertice :both "foo")) => []
    (map get-id (get-edges first-vertice :both "created")) => ["9" "11" "12"]
    (map get-id (get-edges first-vertice :both "something")) => ["1"]
    )

  (facts "about get-vertex"
    (get-id (get-vertex first-edge :in)) => "5"
    (get-id (get-vertex first-edge :out)) => "4")

  (facts "about get-ends"
    (map get-id (get-ends first-vertice :in)) => ["1" "4" "6"]
    (map get-id (get-ends first-vertice :out)) => ["0" "0" "0" "0" "0" "0" "0" "0" "0"]
    (map get-id (get-ends first-vertice :both)) => ["1" "4" "6" "0" "0" "0" "0" "0" "0" "0" "0" "0"]

    (map get-id (get-ends first-vertice :in "foo")) => []
    (map get-id (get-ends first-vertice :in "created")) => ["1" "4" "6"]
    (map get-id (get-ends first-vertice :in "new")) => []
    (map get-id (get-ends first-vertice :in "something")) => []

    (map get-id (get-ends first-vertice :out "foo")) => []
    (map get-id (get-ends first-vertice :out "created")) => []
    (map get-id (get-ends first-vertice :out "new")) => ["0" "0" "0" "0" "0" "0" "0" "0"]
    (map get-id (get-ends first-vertice :out "something")) => ["0"]

    (map get-id (get-ends first-vertice :both "foo")) => []
    (map get-id (get-ends first-vertice :both "created")) => ["1" "4" "6"]
    (map get-id (get-ends first-vertice :both "new")) => ["0" "0" "0" "0" "0" "0" "0" "0"]
    (map get-id (get-ends first-vertice :both "something")) => ["0"]
    )

  (facts "about get-link"
    (get-link first-vertice first-vertice) => nil
    (get-id (get-link first-vertice new-node)) => "21"
    (get-id (get-link new-node first-vertice)) => "21"
    (get-link new-node (second vs)) => nil)

  (facts "about linked?"
    (linked? first-vertice first-vertice) => false
    (linked? first-vertice new-node) => true
    (linked? first-vertice (second vs)) => false)

  (facts "about unlink!"
    (with-db (TinkerGraphFactory/createTinkerGraph)
      (let [v1 (load-vertex "4")
            v2 (load-vertex "5")]
        (unlink! v1 v2)
        (map get-id (get-edges v1 :out)) => ["11"]

    )))

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
