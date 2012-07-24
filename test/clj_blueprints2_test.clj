(ns clj-blueprints2-test
  (:use clj-blueprints2
        midje.sweet
        [fs.core :only (delete-dir)])
  (:require (clj-json [core :as json]))
  (:import (com.tinkerpop.blueprints Vertex Edge Direction Graph)
           (com.tinkerpop.blueprints.impls.tg TinkerGraph TinkerGraphFactory)
           (com.tinkerpop.blueprints.impls.neo4j Neo4jGraph)
           (com.tinkerpop.blueprints.util.wrappers.readonly ReadOnlyGraph)
           (com.tinkerpop.blueprints.util.wrappers.event EventGraph)
           (java.io ByteArrayOutputStream ByteArrayInputStream))
  )

(defn instance-of [k]
  (fn [other] (instance? k other)))

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

  (facts "about with-tx"
    (with-db (Neo4jGraph. "tmp_graph")
      (with-tx
        (vertex nil {"foo" "bar"}))
      (as-map (first (get-vertices))) => {:foo "bar"}

      (try
        (with-tx
          (vertex nil {"stuff" "mux"})
          (throw (RuntimeException.)))
        (catch Exception _))
      (map as-map (get-vertices)) => [{:foo "bar"}]
      )
    (delete-dir "tmp_graph")
    )

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
        (.getVertices g2)) => empty?)
    (let [g2 (TinkerGraphFactory/createTinkerGraph)]
      (clear! g2)
      (.getVertices g2)) => empty?)

  (facts "about get-vertices"
    (with-db g
      (map get-id (get-vertices))) => ["19" "11" "12" "3" "20" "2" "1" "10" "0" "7" "6" "5" "4" "9" "8"]
    (map get-id (get-vertices g)) => ["19" "11" "12" "3" "20" "2" "1" "10" "0" "7" "6" "5" "4" "9" "8"]
      )

  (facts "about get-edges"
    (with-db g
      (map get-id (get-edges))) => ["22" "17" "18" "15" "16" "13" "14" "11" "12" "21" "10" "1" "7" "9" "8"]
    (map get-id (get-edges g)) => ["22" "17" "18" "15" "16" "13" "14" "11" "12" "21" "10" "1" "7" "9" "8"]
    )

  (facts "about load-vertex"
    (with-db g
      (as-map (load-vertex 12)) => {:foo 42
                                    :bar 55})
    (as-map (load-vertex g 12)) => {:foo 42
                                    :bar 55}
    )

  (facts "about load-edge"
    (with-db g
      (as-map (load-edge 17)) => {:foo :bar
                                  :bar :ofo})
    (as-map (load-edge g 17)) => {:foo :bar
                                  :bar :ofo})

  (facts "about vertex"
    (with-db (TinkerGraphFactory/createTinkerGraph)
      (vertex) => (fn [other] (instance-of Vertex))
      (get-id (vertex 42)) => "42"
      (as-map (vertex {:foo 14 :bar 53})) => {:foo 14 :bar 53}
      (get-id (vertex 43 {})) => "43"
      (get-id (vertex 45 nil)) => "45"
      (as-map (vertex 44 {:aba 15})) => {:aba 15})

    (let [g (TinkerGraphFactory/createTinkerGraph)]
      (vertex g) => (fn [other] (instance-of Vertex))
      (get-id (vertex g 42)) => "42"
      (as-map (vertex g nil {:foo 14 :bar 53})) => {:foo 14 :bar 53}
      (get-id (vertex g 43 {})) => "43"
      (get-id (vertex g 45 nil)) => "45"
      (as-map (vertex g 44 {:aba 15})) => {:aba 15}))

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
        ))

    (let [g2 (TinkerGraphFactory/createTinkerGraph)
          vs (.getVertices g2)
          v1 (first vs)
          v2 (second vs)]
      (.getLabel (link! g2 v1 :foo v2)) => "foo"
      (.getVertex (link! g2 v1 :foo v2) Direction/IN) => v2
      (.getVertex (link! g2 v1 :foo v2) Direction/OUT) => v1
      (as-map (link! g2 v1 :bar {:blarg 55} v2)) => {:blarg 55}
      (get-id (link! g2 54465 v1 :quux {:blarg 4354} v2)) => "54465"
      ))

  (facts "about remove!"
    (let [g2 (TinkerGraphFactory/createTinkerGraph)
          v (first (.getVertices g2))
          e (first (.getEdges g2))]
      (with-db g2
        (do (remove! v)
          (.getVertex g2 (get-id v))) => nil
        (do (remove! e)
          (.getEdge g2 (get-id e))) => nil
          ))
    (let [g2 (TinkerGraphFactory/createTinkerGraph)
          v (first (.getVertices g2))
          e (first (.getEdges g2))]
      (do (remove! g2 v)
          (.getVertex g2 (get-id v))) => nil
      (do (remove! g2 e)
          (.getEdge g2 (get-id e))) => nil
          )
    )

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

  (facts "about create-key-index! and get-key-indices"
    (create-key-index! :name Vertex)
    (seq (get-key-indices Vertex)) => ["name"]

    (create-key-index! :blarg Edge)
    (create-key-index! :cho Edge)
    (seq (get-key-indices Edge)) => ["blarg" "cho"]

    (let [g2 (TinkerGraphFactory/createTinkerGraph)]
      (create-key-index! g2 :name Vertex)
      (seq (get-key-indices g2 Vertex)) => ["name"]

      (create-key-index! g2 :blarg Edge)
      (create-key-index! g2 :cho Edge)
      (seq (get-key-indices g2 Edge)) => ["blarg" "cho"]
      )
    )

  (facts "about create-index! and get-indices"
    (let [ix  (create-index! :fox Vertex)
          ix2 (create-index! :blue Vertex)]
      (seq (get-indices)) => (just #{ix ix2})

      (let [ix3 (create-index! :fox2 Edge)
            ix4 (create-index! :blue2 Edge)
            ix5 (create-index! :green4 Edge)
            ]
        (seq (get-indices)) => (just #{ix ix2 ix3 ix4 ix5})))

    (let [g2 (TinkerGraphFactory/createTinkerGraph)
          ix  (create-index! g2 :fox Vertex)
          ix2 (create-index! g2 :blue Vertex)]
      (seq (get-indices g2)) => (just #{ix ix2})

      (let [ix3 (create-index! g2 :fox2 Edge)
            ix4 (create-index! g2 :blue2 Edge)
            ix5 (create-index! g2 :green4 Edge)
            ]
        (seq (get-indices g2)) => (just #{ix ix2 ix3 ix4 ix5})))
    )

  (facts "about get-index"
    (let [ix (create-index! :flo Vertex)]
      (get-index :flo Vertex) => ix)

    (let [ix  (create-index! :mux Edge)]
      (get-index :mux Edge) => ix)

    (let [g2 (TinkerGraphFactory/createTinkerGraph)]
      (let [ix (create-index! g2 :flo Vertex)]
        (get-index g2 :flo Vertex) => ix)

      (let [ix  (create-index! g2 :mux Edge)]
        (get-index g2 :mux Edge) => ix)
      )
    )

  (facts "about drop-index!"
    (let [ix (create-index! :flo2 Vertex)]
      (drop-index! :flo2)
      (get-index :flo2 Vertex) => nil)

    (let [ix (create-index! :lof Edge)]
      (drop-index! :lof)
      (get-index :lof Edge) => nil)

    (let [g2 (TinkerGraphFactory/createTinkerGraph)]
      (let [ix (create-index! g2 :flo2 Vertex)]
        (drop-index! g2 :flo2)
        (get-index g2 :flo2 Vertex) => nil)

      (let [ix (create-index! g2 :lof Edge)]
        (drop-index! g2 :lof)
        (get-index g2 :lof Edge) => nil))
    )

  (facts "about index-class"
    (let [ix (create-index! :flo3 Vertex)]
      (index-class ix) => Vertex)

    (let [ix (create-index! :flo4 Edge)]
      (index-class ix) => Edge)
    )

  (facts "about index-name"
    (let [ix (create-index! :flo5 Vertex)]
      (index-name ix) => "flo5")

    (let [ix (create-index! :flo6 Edge)]
      (index-name ix) => "flo6")
    )

  (facts "about iget"
    (let [ix (create-index! :hmm Vertex)]
      (.put ix "something" "fun" first-vertice)
      (.put ix "something" "other" (second vs))

      (first (iget ix "something" "foamy")) => nil
      (first (iget ix "foo" "foamy")) => nil
      (first (iget ix "something" "fun")) => first-vertice
      (first (iget ix "something" "other")) => (second vs))

    (let [ix (create-index! :stuff Edge)]
      (.put ix "something" "fun" first-edge)
      (.put ix "something" "other" (second es))

      (first (iget ix "something" "foamy")) => nil
      (first (iget ix "foo" "foamy")) => nil
      (first (iget ix "something" "fun")) => first-edge
      (first (iget ix "something" "other")) => (second es))
    )

  (facts "about iput"
    (let [ix (create-index! :foo1 Vertex)]
      (iput ix "hello" "blarg" first-vertice)
      (iput ix "something" "blarg" (second vs))

      (first (.get ix "hello" "blarg")) => first-vertice
      (first (.get ix "something" "blarg")) => (second vs))

    (let [ix (create-index! :foo2 Edge)]
      (iput ix "hello" "blarg" first-edge)
      (iput ix "something" "blarg" (second es))

      (first (.get ix "hello" "blarg")) => first-edge
      (first (.get ix "something" "blarg")) => (second es))
    )

  (facts "about iremove"
    (let [ix (create-index! :foo3 Vertex)]
      (.put ix "hello" "blarg" first-vertice)
      (.put ix "something" "blarg" (second vs))

      (iremove ix "hello" "blarg" first-vertice)
      (iremove ix "something" "blarg" (second vs))

      (first (.get ix "hello" "blarg")) => nil
      (first (.get ix "something" "blarg")) => nil)

    (let [ix (create-index! :foo4 Edge)]
      (.put ix "hello" "blarg" first-edge)
      (.put ix "something" "blarg" (second es))

      (iremove ix "hello" "blarg" first-edge)
      (iremove ix "something" "blarg" (second es))

      (first (.get ix "hello" "blarg")) => nil
      (first (.get ix "something" "blarg")) => nil)
    )

  (facts "about as-read-only"
    (with-db *db*
      (as-read-only) => (instance-of ReadOnlyGraph))

    (as-read-only g) => (instance-of ReadOnlyGraph))

  (facts "about migrate-graph!"
    (let [g2 (TinkerGraph.)]
      (migrate-graph! g g2)
      (map get-id (get-vertices)) => ["3" "2" "1" "6" "5" "4"]))

  (facts "about read-graph-ml!"
    (let [test-str "<?xml version=\"1.0\" ?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">
    <key id=\"age\" for=\"node\" attr.name=\"age\" attr.type=\"int\"></key>
    <key id=\"bar\" for=\"node\" attr.name=\"bar\" attr.type=\"string\"></key>
    <key id=\"foo\" for=\"node\" attr.name=\"foo\" attr.type=\"string\"></key>
    <key id=\"lang\" for=\"node\" attr.name=\"lang\" attr.type=\"string\"></key>
    <key id=\"name\" for=\"node\" attr.name=\"name\" attr.type=\"string\"></key>
    <key id=\"one\" for=\"node\" attr.name=\"one\" attr.type=\"string\"></key>
    <key id=\"three\" for=\"node\" attr.name=\"three\" attr.type=\"long\"></key>
    <key id=\"two\" for=\"node\" attr.name=\"two\" attr.type=\"string\"></key>
    <key id=\"bar\" for=\"edge\" attr.name=\"bar\" attr.type=\"string\"></key>
    <key id=\"five\" for=\"edge\" attr.name=\"five\" attr.type=\"string\"></key>
    <key id=\"foo\" for=\"edge\" attr.name=\"foo\" attr.type=\"string\"></key>
    <key id=\"four\" for=\"edge\" attr.name=\"four\" attr.type=\"string\"></key>
    <key id=\"six\" for=\"edge\" attr.name=\"six\" attr.type=\"long\"></key>
    <key id=\"weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"float\"></key>
    <graph id=\"G\" edgedefault=\"directed\">
        <node id=\"0\">
            <data key=\"one\">foo</data>
            <data key=\"three\">142</data>
            <data key=\"two\">:bar</data>
        </node>
        <node id=\"1\">
            <data key=\"age\">29</data>
            <data key=\"name\">marko</data>
        </node>
        <node id=\"10\">
            <data key=\"bar\">ofo</data>
            <data key=\"foo\">bar</data>
        </node>
        <node id=\"11\">
            <data key=\"bar\">:ofo</data>
            <data key=\"foo\">:bar</data>
        </node>
        <node id=\"12\">
            <data key=\"bar\">55</data>
            <data key=\"foo\">42</data>
        </node>
        <node id=\"19\"></node>
        <node id=\"2\">
            <data key=\"age\">27</data>
            <data key=\"name\">vadas</data>
        </node>
        <node id=\"20\"></node>
        <node id=\"3\">
            <data key=\"lang\">java</data>
            <data key=\"name\">lop</data>
        </node>
        <node id=\"4\">
            <data key=\"age\">32</data>
            <data key=\"name\">josh</data>
        </node>
        <node id=\"5\">
            <data key=\"lang\">java</data>
            <data key=\"name\">ripple</data>
        </node>
        <node id=\"6\">
            <data key=\"age\">35</data>
            <data key=\"name\">peter</data>
        </node>
        <node id=\"7\">
            <data key=\"foo\">bar</data>
        </node>
        <node id=\"8\">
            <data key=\"foo\">:bar</data>
        </node>
        <node id=\"9\">
            <data key=\"foo\">42</data>
        </node>
        <edge id=\"1\" source=\"3\" target=\"0\" label=\"something\">
            <data key=\"five\">:zed</data>
            <data key=\"four\">quux</data>
            <data key=\"six\">55</data>
        </edge>
        <edge id=\"10\" source=\"4\" target=\"5\" label=\"created\">
            <data key=\"weight\">1.0</data>
        </edge>
        <edge id=\"11\" source=\"4\" target=\"3\" label=\"created\">
            <data key=\"weight\">0.4</data>
        </edge>
        <edge id=\"12\" source=\"6\" target=\"3\" label=\"created\">
            <data key=\"weight\">0.2</data>
        </edge>
        <edge id=\"13\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"foo\">bar</data>
        </edge>
        <edge id=\"14\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"foo\">:bar</data>
        </edge>
        <edge id=\"15\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"foo\">42</data>
        </edge>
        <edge id=\"16\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"bar\">ofo</data>
            <data key=\"foo\">bar</data>
        </edge>
        <edge id=\"17\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"bar\">:ofo</data>
            <data key=\"foo\">:bar</data>
        </edge>
        <edge id=\"18\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"bar\">55</data>
            <data key=\"foo\">42</data>
        </edge>
        <edge id=\"21\" source=\"3\" target=\"0\" label=\"new\"></edge>
        <edge id=\"22\" source=\"3\" target=\"0\" label=\"new\"></edge>
        <edge id=\"7\" source=\"1\" target=\"2\" label=\"knows\">
            <data key=\"weight\">0.5</data>
        </edge>
        <edge id=\"8\" source=\"1\" target=\"4\" label=\"knows\">
            <data key=\"weight\">1.0</data>
        </edge>
        <edge id=\"9\" source=\"1\" target=\"3\" label=\"created\">
            <data key=\"weight\">0.4</data>
        </edge>
    </graph>
</graphml>"]
      (with-db (TinkerGraph.)
        (read-graph-ml! (ByteArrayInputStream. (.getBytes test-str)))
        (pget (load-vertex "0") "three") => 142)

      (with-db (TinkerGraph.)
        (read-graph-ml! (ByteArrayInputStream. (.getBytes test-str)) :buffer-size 120)
        (pget (load-vertex "0") "three") => 142)

      (with-db (TinkerGraph.)
        (read-graph-ml! (ByteArrayInputStream. (.getBytes test-str)) :vertex-id-key "qqq")
        (pget (load-vertex "0") "three") => 142)

      (with-db (TinkerGraph.)
        (read-graph-ml! (ByteArrayInputStream. (.getBytes test-str)) :edge-id-key "qqq")
        (pget (load-vertex "0") "three") => 142)

      (with-db (TinkerGraph.)
        (read-graph-ml! (ByteArrayInputStream. (.getBytes test-str)) :edge-label-key "qqq")
        (pget (load-vertex "0") "three") => 142)

      ))

  (facts "about write-graph-ml!"
    (with-db g
      (let [bw (ByteArrayOutputStream.)]
        (write-graph-ml! bw)
        (.toString bw)) =>
        (str "<?xml version=\"1.0\" ?>"
"<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">"
"<key id=\"two\" for=\"node\" attr.name=\"two\" attr.type=\"string\">"
"</key>"
"<key id=\"one\" for=\"node\" attr.name=\"one\" attr.type=\"string\">"
"</key>"
"<key id=\"age\" for=\"node\" attr.name=\"age\" attr.type=\"int\">"
"</key>"
"<key id=\"name\" for=\"node\" attr.name=\"name\" attr.type=\"string\">"
"</key>"
"<key id=\"three\" for=\"node\" attr.name=\"three\" attr.type=\"long\">"
"</key>"
"<key id=\"foo\" for=\"node\" attr.name=\"foo\" attr.type=\"string\">"
"</key>"
"<key id=\"bar\" for=\"node\" attr.name=\"bar\" attr.type=\"string\">"
"</key>"
"<key id=\"lang\" for=\"node\" attr.name=\"lang\" attr.type=\"string\">"
"</key>"
"<key id=\"weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"float\">"
"</key>"
"<key id=\"five\" for=\"edge\" attr.name=\"five\" attr.type=\"string\">"
"</key>"
"<key id=\"foo\" for=\"edge\" attr.name=\"foo\" attr.type=\"string\">"
"</key>"
"<key id=\"four\" for=\"edge\" attr.name=\"four\" attr.type=\"string\">"
"</key>"
"<key id=\"bar\" for=\"edge\" attr.name=\"bar\" attr.type=\"string\">"
"</key>"
"<key id=\"six\" for=\"edge\" attr.name=\"six\" attr.type=\"long\">"
"</key>"
"<graph id=\"G\" edgedefault=\"directed\">"
"<node id=\"19\">"
"</node>"
"<node id=\"11\">"
"<data key=\"foo\">:bar</data>"
"<data key=\"bar\">:ofo</data>"
"</node>"
"<node id=\"12\">"
"<data key=\"foo\">42</data>"
"<data key=\"bar\">55</data>"
"</node>"
"<node id=\"3\">"
"<data key=\"name\">lop</data>"
"<data key=\"lang\">java</data>"
"</node>"
"<node id=\"20\">"
"</node>"
"<node id=\"2\">"
"<data key=\"age\">27</data>"
"<data key=\"name\">vadas</data>"
"</node>"
"<node id=\"1\">"
"<data key=\"age\">29</data>"
"<data key=\"name\">marko</data>"
"</node>"
"<node id=\"10\">"
"<data key=\"foo\">bar</data>"
"<data key=\"bar\">ofo</data>"
"</node>"
"<node id=\"0\">"
"<data key=\"two\">:bar</data>"
"<data key=\"one\">foo</data>"
"<data key=\"three\">42</data>"
"</node>"
"<node id=\"7\">"
"<data key=\"foo\">bar</data>"
"</node>"
"<node id=\"6\">"
"<data key=\"age\">35</data>"
"<data key=\"name\">peter</data>"
"</node>"
"<node id=\"5\">"
"<data key=\"name\">ripple</data>"
"<data key=\"lang\">java</data>"
"</node>"
"<node id=\"4\">"
"<data key=\"age\">32</data>"
"<data key=\"name\">josh</data>"
"</node>"
"<node id=\"9\">"
"<data key=\"foo\">42</data>"
"</node>"
"<node id=\"8\">"
"<data key=\"foo\">:bar</data>"
"</node>"
"<edge id=\"21\" source=\"3\" target=\"0\" label=\"new\">"
"</edge>"
"<edge id=\"22\" source=\"3\" target=\"0\" label=\"new\">"
"</edge>"
"<edge id=\"17\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">:bar</data>"
"<data key=\"bar\">:ofo</data>"
"</edge>"
"<edge id=\"18\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">42</data>"
"<data key=\"bar\">55</data>"
"</edge>"
"<edge id=\"15\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">42</data>"
"</edge>"
"<edge id=\"16\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">bar</data>"
"<data key=\"bar\">ofo</data>"
"</edge>"
"<edge id=\"13\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">bar</data>"
"</edge>"
"<edge id=\"14\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">:bar</data>"
"</edge>"
"<edge id=\"1\" source=\"3\" target=\"0\" label=\"something\">"
"<data key=\"five\">:zed</data>"
"<data key=\"four\">quux</data>"
"<data key=\"six\">55</data>"
"</edge>"
"<edge id=\"7\" source=\"1\" target=\"2\" label=\"knows\">"
"<data key=\"weight\">0.5</data>"
"</edge>"
"<edge id=\"8\" source=\"1\" target=\"4\" label=\"knows\">"
"<data key=\"weight\">1.0</data>"
"</edge>"
"<edge id=\"9\" source=\"1\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.4</data>"
"</edge>"
"<edge id=\"12\" source=\"6\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.2</data>"
"</edge>"
"<edge id=\"10\" source=\"4\" target=\"5\" label=\"created\">"
"<data key=\"weight\">1.0</data>"
"</edge>"
"<edge id=\"11\" source=\"4\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.4</data>"
"</edge>"
"</graph>"
"</graphml>")

      (let [bw (ByteArrayOutputStream.)]
        (write-graph-ml! bw :vertex-types {"two" "int"})
        (.toString bw)) =>
        (str "<?xml version=\"1.0\" ?>"
"<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">"
"<key id=\"two\" for=\"node\" attr.name=\"two\" attr.type=\"int\">"
"</key>"
"<key id=\"weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"float\">"
"</key>"
"<key id=\"five\" for=\"edge\" attr.name=\"five\" attr.type=\"string\">"
"</key>"
"<key id=\"foo\" for=\"edge\" attr.name=\"foo\" attr.type=\"string\">"
"</key>"
"<key id=\"four\" for=\"edge\" attr.name=\"four\" attr.type=\"string\">"
"</key>"
"<key id=\"bar\" for=\"edge\" attr.name=\"bar\" attr.type=\"string\">"
"</key>"
"<key id=\"six\" for=\"edge\" attr.name=\"six\" attr.type=\"long\">"
"</key>"
"<graph id=\"G\" edgedefault=\"directed\">"
"<node id=\"19\">"
"</node>"
"<node id=\"11\">"
"<data key=\"foo\">:bar</data>"
"<data key=\"bar\">:ofo</data>"
"</node>"
"<node id=\"12\">"
"<data key=\"foo\">42</data>"
"<data key=\"bar\">55</data>"
"</node>"
"<node id=\"3\">"
"<data key=\"name\">lop</data>"
"<data key=\"lang\">java</data>"
"</node>"
"<node id=\"20\">"
"</node>"
"<node id=\"2\">"
"<data key=\"age\">27</data>"
"<data key=\"name\">vadas</data>"
"</node>"
"<node id=\"1\">"
"<data key=\"age\">29</data>"
"<data key=\"name\">marko</data>"
"</node>"
"<node id=\"10\">"
"<data key=\"foo\">bar</data>"
"<data key=\"bar\">ofo</data>"
"</node>"
"<node id=\"0\">"
"<data key=\"two\">:bar</data>"
"<data key=\"one\">foo</data>"
"<data key=\"three\">42</data>"
"</node>"
"<node id=\"7\">"
"<data key=\"foo\">bar</data>"
"</node>"
"<node id=\"6\">"
"<data key=\"age\">35</data>"
"<data key=\"name\">peter</data>"
"</node>"
"<node id=\"5\">"
"<data key=\"name\">ripple</data>"
"<data key=\"lang\">java</data>"
"</node>"
"<node id=\"4\">"
"<data key=\"age\">32</data>"
"<data key=\"name\">josh</data>"
"</node>"
"<node id=\"9\">"
"<data key=\"foo\">42</data>"
"</node>"
"<node id=\"8\">"
"<data key=\"foo\">:bar</data>"
"</node>"
"<edge id=\"21\" source=\"3\" target=\"0\" label=\"new\">"
"</edge>"
"<edge id=\"22\" source=\"3\" target=\"0\" label=\"new\">"
"</edge>"
"<edge id=\"17\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">:bar</data>"
"<data key=\"bar\">:ofo</data>"
"</edge>"
"<edge id=\"18\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">42</data>"
"<data key=\"bar\">55</data>"
"</edge>"
"<edge id=\"15\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">42</data>"
"</edge>"
"<edge id=\"16\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">bar</data>"
"<data key=\"bar\">ofo</data>"
"</edge>"
"<edge id=\"13\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">bar</data>"
"</edge>"
"<edge id=\"14\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">:bar</data>"
"</edge>"
"<edge id=\"1\" source=\"3\" target=\"0\" label=\"something\">"
"<data key=\"five\">:zed</data>"
"<data key=\"four\">quux</data>"
"<data key=\"six\">55</data>"
"</edge>"
"<edge id=\"7\" source=\"1\" target=\"2\" label=\"knows\">"
"<data key=\"weight\">0.5</data>"
"</edge>"
"<edge id=\"8\" source=\"1\" target=\"4\" label=\"knows\">"
"<data key=\"weight\">1.0</data>"
"</edge>"
"<edge id=\"9\" source=\"1\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.4</data>"
"</edge>"
"<edge id=\"12\" source=\"6\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.2</data>"
"</edge>"
"<edge id=\"10\" source=\"4\" target=\"5\" label=\"created\">"
"<data key=\"weight\">1.0</data>"
"</edge>"
"<edge id=\"11\" source=\"4\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.4</data>"
"</edge>"
"</graph>"
"</graphml>")

      (let [bw (ByteArrayOutputStream.)]
        (write-graph-ml! bw :edge-types {"foo" "string"})
        (.toString bw)) =>
        (str "<?xml version=\"1.0\" ?>"
"<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">"
"<key id=\"two\" for=\"node\" attr.name=\"two\" attr.type=\"string\">"
"</key>"
"<key id=\"one\" for=\"node\" attr.name=\"one\" attr.type=\"string\">"
"</key>"
"<key id=\"age\" for=\"node\" attr.name=\"age\" attr.type=\"int\">"
"</key>"
"<key id=\"name\" for=\"node\" attr.name=\"name\" attr.type=\"string\">"
"</key>"
"<key id=\"three\" for=\"node\" attr.name=\"three\" attr.type=\"long\">"
"</key>"
"<key id=\"foo\" for=\"node\" attr.name=\"foo\" attr.type=\"string\">"
"</key>"
"<key id=\"bar\" for=\"node\" attr.name=\"bar\" attr.type=\"string\">"
"</key>"
"<key id=\"lang\" for=\"node\" attr.name=\"lang\" attr.type=\"string\">"
"</key>"
"<key id=\"foo\" for=\"edge\" attr.name=\"foo\" attr.type=\"string\">"
"</key>"
"<graph id=\"G\" edgedefault=\"directed\">"
"<node id=\"19\">"
"</node>"
"<node id=\"11\">"
"<data key=\"foo\">:bar</data>"
"<data key=\"bar\">:ofo</data>"
"</node>"
"<node id=\"12\">"
"<data key=\"foo\">42</data>"
"<data key=\"bar\">55</data>"
"</node>"
"<node id=\"3\">"
"<data key=\"name\">lop</data>"
"<data key=\"lang\">java</data>"
"</node>"
"<node id=\"20\">"
"</node>"
"<node id=\"2\">"
"<data key=\"age\">27</data>"
"<data key=\"name\">vadas</data>"
"</node>"
"<node id=\"1\">"
"<data key=\"age\">29</data>"
"<data key=\"name\">marko</data>"
"</node>"
"<node id=\"10\">"
"<data key=\"foo\">bar</data>"
"<data key=\"bar\">ofo</data>"
"</node>"
"<node id=\"0\">"
"<data key=\"two\">:bar</data>"
"<data key=\"one\">foo</data>"
"<data key=\"three\">42</data>"
"</node>"
"<node id=\"7\">"
"<data key=\"foo\">bar</data>"
"</node>"
"<node id=\"6\">"
"<data key=\"age\">35</data>"
"<data key=\"name\">peter</data>"
"</node>"
"<node id=\"5\">"
"<data key=\"name\">ripple</data>"
"<data key=\"lang\">java</data>"
"</node>"
"<node id=\"4\">"
"<data key=\"age\">32</data>"
"<data key=\"name\">josh</data>"
"</node>"
"<node id=\"9\">"
"<data key=\"foo\">42</data>"
"</node>"
"<node id=\"8\">"
"<data key=\"foo\">:bar</data>"
"</node>"
"<edge id=\"21\" source=\"3\" target=\"0\" label=\"new\">"
"</edge>"
"<edge id=\"22\" source=\"3\" target=\"0\" label=\"new\">"
"</edge>"
"<edge id=\"17\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">:bar</data>"
"<data key=\"bar\">:ofo</data>"
"</edge>"
"<edge id=\"18\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">42</data>"
"<data key=\"bar\">55</data>"
"</edge>"
"<edge id=\"15\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">42</data>"
"</edge>"
"<edge id=\"16\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">bar</data>"
"<data key=\"bar\">ofo</data>"
"</edge>"
"<edge id=\"13\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">bar</data>"
"</edge>"
"<edge id=\"14\" source=\"3\" target=\"0\" label=\"new\">"
"<data key=\"foo\">:bar</data>"
"</edge>"
"<edge id=\"1\" source=\"3\" target=\"0\" label=\"something\">"
"<data key=\"five\">:zed</data>"
"<data key=\"four\">quux</data>"
"<data key=\"six\">55</data>"
"</edge>"
"<edge id=\"7\" source=\"1\" target=\"2\" label=\"knows\">"
"<data key=\"weight\">0.5</data>"
"</edge>"
"<edge id=\"8\" source=\"1\" target=\"4\" label=\"knows\">"
"<data key=\"weight\">1.0</data>"
"</edge>"
"<edge id=\"9\" source=\"1\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.4</data>"
"</edge>"
"<edge id=\"12\" source=\"6\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.2</data>"
"</edge>"
"<edge id=\"10\" source=\"4\" target=\"5\" label=\"created\">"
"<data key=\"weight\">1.0</data>"
"</edge>"
"<edge id=\"11\" source=\"4\" target=\"3\" label=\"created\">"
"<data key=\"weight\">0.4</data>"
"</edge>"
"</graph>"
"</graphml>")

      (let [bw (ByteArrayOutputStream.)]
        (write-graph-ml! bw :normalize true)
        (.toString bw)) =>
        "<?xml version=\"1.0\" ?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">
    <key id=\"age\" for=\"node\" attr.name=\"age\" attr.type=\"int\"></key>
    <key id=\"bar\" for=\"node\" attr.name=\"bar\" attr.type=\"string\"></key>
    <key id=\"foo\" for=\"node\" attr.name=\"foo\" attr.type=\"string\"></key>
    <key id=\"lang\" for=\"node\" attr.name=\"lang\" attr.type=\"string\"></key>
    <key id=\"name\" for=\"node\" attr.name=\"name\" attr.type=\"string\"></key>
    <key id=\"one\" for=\"node\" attr.name=\"one\" attr.type=\"string\"></key>
    <key id=\"three\" for=\"node\" attr.name=\"three\" attr.type=\"long\"></key>
    <key id=\"two\" for=\"node\" attr.name=\"two\" attr.type=\"string\"></key>
    <key id=\"bar\" for=\"edge\" attr.name=\"bar\" attr.type=\"string\"></key>
    <key id=\"five\" for=\"edge\" attr.name=\"five\" attr.type=\"string\"></key>
    <key id=\"foo\" for=\"edge\" attr.name=\"foo\" attr.type=\"string\"></key>
    <key id=\"four\" for=\"edge\" attr.name=\"four\" attr.type=\"string\"></key>
    <key id=\"six\" for=\"edge\" attr.name=\"six\" attr.type=\"long\"></key>
    <key id=\"weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"float\"></key>
    <graph id=\"G\" edgedefault=\"directed\">
        <node id=\"0\">
            <data key=\"one\">foo</data>
            <data key=\"three\">42</data>
            <data key=\"two\">:bar</data>
        </node>
        <node id=\"1\">
            <data key=\"age\">29</data>
            <data key=\"name\">marko</data>
        </node>
        <node id=\"10\">
            <data key=\"bar\">ofo</data>
            <data key=\"foo\">bar</data>
        </node>
        <node id=\"11\">
            <data key=\"bar\">:ofo</data>
            <data key=\"foo\">:bar</data>
        </node>
        <node id=\"12\">
            <data key=\"bar\">55</data>
            <data key=\"foo\">42</data>
        </node>
        <node id=\"19\"></node>
        <node id=\"2\">
            <data key=\"age\">27</data>
            <data key=\"name\">vadas</data>
        </node>
        <node id=\"20\"></node>
        <node id=\"3\">
            <data key=\"lang\">java</data>
            <data key=\"name\">lop</data>
        </node>
        <node id=\"4\">
            <data key=\"age\">32</data>
            <data key=\"name\">josh</data>
        </node>
        <node id=\"5\">
            <data key=\"lang\">java</data>
            <data key=\"name\">ripple</data>
        </node>
        <node id=\"6\">
            <data key=\"age\">35</data>
            <data key=\"name\">peter</data>
        </node>
        <node id=\"7\">
            <data key=\"foo\">bar</data>
        </node>
        <node id=\"8\">
            <data key=\"foo\">:bar</data>
        </node>
        <node id=\"9\">
            <data key=\"foo\">42</data>
        </node>
        <edge id=\"1\" source=\"3\" target=\"0\" label=\"something\">
            <data key=\"five\">:zed</data>
            <data key=\"four\">quux</data>
            <data key=\"six\">55</data>
        </edge>
        <edge id=\"10\" source=\"4\" target=\"5\" label=\"created\">
            <data key=\"weight\">1.0</data>
        </edge>
        <edge id=\"11\" source=\"4\" target=\"3\" label=\"created\">
            <data key=\"weight\">0.4</data>
        </edge>
        <edge id=\"12\" source=\"6\" target=\"3\" label=\"created\">
            <data key=\"weight\">0.2</data>
        </edge>
        <edge id=\"13\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"foo\">bar</data>
        </edge>
        <edge id=\"14\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"foo\">:bar</data>
        </edge>
        <edge id=\"15\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"foo\">42</data>
        </edge>
        <edge id=\"16\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"bar\">ofo</data>
            <data key=\"foo\">bar</data>
        </edge>
        <edge id=\"17\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"bar\">:ofo</data>
            <data key=\"foo\">:bar</data>
        </edge>
        <edge id=\"18\" source=\"3\" target=\"0\" label=\"new\">
            <data key=\"bar\">55</data>
            <data key=\"foo\">42</data>
        </edge>
        <edge id=\"21\" source=\"3\" target=\"0\" label=\"new\"></edge>
        <edge id=\"22\" source=\"3\" target=\"0\" label=\"new\"></edge>
        <edge id=\"7\" source=\"1\" target=\"2\" label=\"knows\">
            <data key=\"weight\">0.5</data>
        </edge>
        <edge id=\"8\" source=\"1\" target=\"4\" label=\"knows\">
            <data key=\"weight\">1.0</data>
        </edge>
        <edge id=\"9\" source=\"1\" target=\"3\" label=\"created\">
            <data key=\"weight\">0.4</data>
        </edge>
    </graph>
</graphml>"
        ))


  (facts "about read-graph-json!"
    (let [test-str "{\"vertices\":[{\"_type\":\"vertex\",\"_id\":\"19\"},{\"_type\":\"vertex\",\"_id\":\"8\"}],\"edges\":[{\"_outV\":\"19\",\"_inV\":\"8\",\"_label\":\"new\",\"_type\":\"edge\",\"_id\":\"22\"}]}"]
      (with-db (TinkerGraph.)
        (read-graph-json! (ByteArrayInputStream. (.getBytes test-str)))
        (count (seq (get-edges (load-vertex "19") :both))) => 1)

      (with-db (TinkerGraph.)
        (read-graph-json! (ByteArrayInputStream. (.getBytes test-str)) 1024)
        (count (seq (get-edges (load-vertex "19") :both))) => 1)

      )
    )

  (facts "about write-graph-json!"
    (with-db g
      (let [bw (ByteArrayOutputStream.)]
        (write-graph-json! bw)
        (json/parse-string (.toString bw))) => {"vertices" [{"_id" "19", "_type" "vertex"}
                                                            {"_id" "11", "_type" "vertex"}
                                                            {"_id" "12", "_type" "vertex"}
                                                            {"_id" "3", "_type" "vertex"}
                                                            {"_id" "20", "_type" "vertex"}
                                                            {"_id" "2", "_type" "vertex"}
                                                            {"_id" "1", "_type" "vertex"}
                                                            {"_id" "10", "_type" "vertex"}
                                                            {"_id" "0", "_type" "vertex"}
                                                            {"_id" "7", "_type" "vertex"}
                                                            {"_id" "6", "_type" "vertex"}
                                                            {"_id" "5", "_type" "vertex"}
                                                            {"_id" "4", "_type" "vertex"}
                                                            {"_id" "9", "_type" "vertex"}
                                                            {"_id" "8", "_type" "vertex"}],
                                                "edges" [{"_id" "22", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "17", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "18", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "15", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "16", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "13", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "14", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "11", "_type" "edge", "_outV" "4", "_inV" "3", "_label" "created"}
                                                         {"_id" "12", "_type" "edge", "_outV" "6", "_inV" "3", "_label" "created"}
                                                         {"_id" "21", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                         {"_id" "10", "_type" "edge", "_outV" "4", "_inV" "5", "_label" "created"}
                                                         {"_id" "1", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "something"}
                                                         {"_id" "7", "_type" "edge", "_outV" "1", "_inV" "2", "_label" "knows"}
                                                         {"_id" "9", "_type" "edge", "_outV" "1", "_inV" "3", "_label" "created"}
                                                         {"_id" "8", "_type" "edge", "_outV" "1", "_inV" "4", "_label" "knows"}]}

        (let [bw (ByteArrayOutputStream.)]
          (write-graph-json! bw :show-types true)
          (json/parse-string (.toString bw))) => {"embeddedTypes" true, "vertices" [{"_id" "19", "_type" "vertex"}
                                                                                    {"_id" "11", "_type" "vertex"}
                                                                                    {"_id" "12", "_type" "vertex"}
                                                                                    {"_id" "3", "_type" "vertex"}
                                                                                    {"_id" "20", "_type" "vertex"}
                                                                                    {"_id" "2", "_type" "vertex"}
                                                                                    {"_id" "1", "_type" "vertex"}
                                                                                    {"_id" "10", "_type" "vertex"}
                                                                                    {"_id" "0", "_type" "vertex"}
                                                                                    {"_id" "7", "_type" "vertex"}
                                                                                    {"_id" "6", "_type" "vertex"}
                                                                                    {"_id" "5", "_type" "vertex"}
                                                                                    {"_id" "4", "_type" "vertex"}
                                                                                    {"_id" "9", "_type" "vertex"}
                                                                                    {"_id" "8", "_type" "vertex"}],
                                                  "edges" [{"_id" "22", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "17", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "18", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "15", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "16", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "13", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "14", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "11", "_type" "edge", "_outV" "4", "_inV" "3", "_label" "created"}
                                                           {"_id" "12", "_type" "edge", "_outV" "6", "_inV" "3", "_label" "created"}
                                                           {"_id" "21", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                           {"_id" "10", "_type" "edge", "_outV" "4", "_inV" "5", "_label" "created"}
                                                           {"_id" "1", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "something"}
                                                           {"_id" "7", "_type" "edge", "_outV" "1", "_inV" "2", "_label" "knows"}
                                                           {"_id" "9", "_type" "edge", "_outV" "1", "_inV" "3", "_label" "created"}
                                                           {"_id" "8", "_type" "edge", "_outV" "1", "_inV" "4", "_label" "knows"}]}

          (let [bw (ByteArrayOutputStream.)]
            (write-graph-json! bw :vertex-props [:name])
            (json/parse-string (.toString bw))) => {"vertices" [{"_id" "19", "_type" "vertex"}
                                                                {"_id" "11", "_type" "vertex"}
                                                                {"_id" "12", "_type" "vertex"}
                                                                {"name" "lop", "_id" "3", "_type" "vertex"}
                                                                {"_id" "20", "_type" "vertex"}
                                                                {"name" "vadas", "_id" "2", "_type" "vertex"}
                                                                {"name" "marko", "_id" "1", "_type" "vertex"}
                                                                {"_id" "10", "_type" "vertex"}
                                                                {"_id" "0", "_type" "vertex"}
                                                                {"_id" "7", "_type" "vertex"}
                                                                {"name" "peter", "_id" "6", "_type" "vertex"}
                                                                {"name" "ripple", "_id" "5", "_type" "vertex"}
                                                                {"name" "josh", "_id" "4", "_type" "vertex"}
                                                                {"_id" "9", "_type" "vertex"}
                                                                {"_id" "8", "_type" "vertex"}],
                                                    "edges" [{"_id" "22", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "17", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "18", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "15", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "16", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "13", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "14", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "11", "_type" "edge", "_outV" "4", "_inV" "3", "_label" "created"}
                                                             {"_id" "12", "_type" "edge", "_outV" "6", "_inV" "3", "_label" "created"}
                                                             {"_id" "21", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                                                             {"_id" "10", "_type" "edge", "_outV" "4", "_inV" "5", "_label" "created"}
                                                             {"_id" "1", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "something"}
                                                             {"_id" "7", "_type" "edge", "_outV" "1", "_inV" "2", "_label" "knows"}
                                                             {"_id" "9", "_type" "edge", "_outV" "1", "_inV" "3", "_label" "created"}
                                                             {"_id" "8", "_type" "edge", "_outV" "1", "_inV" "4", "_label" "knows"}]}

      (let [bw (ByteArrayOutputStream.)]
        (write-graph-json! bw :edge-props [:weight])
        (json/parse-string (.toString bw))) =>
        {"vertices" [{"_id" "19", "_type" "vertex"}
                     {"_id" "11", "_type" "vertex"}
                     {"_id" "12", "_type" "vertex"}
                     {"_id" "3", "_type" "vertex"}
                     {"_id" "20", "_type" "vertex"}
                     {"_id" "2", "_type" "vertex"}
                     {"_id" "1", "_type" "vertex"}
                     {"_id" "10", "_type" "vertex"}
                     {"_id" "0", "_type" "vertex"}
                     {"_id" "7", "_type" "vertex"}
                     {"_id" "6", "_type" "vertex"}
                     {"_id" "5", "_type" "vertex"}
                     {"_id" "4", "_type" "vertex"}
                     {"_id" "9", "_type" "vertex"}
                     {"_id" "8", "_type" "vertex"}],
         "edges" [{"_id" "22", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"_id" "17", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"_id" "18", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"_id" "15", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"_id" "16", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"_id" "13", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"_id" "14", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"weight" 0.4000000059604645, "_id" "11", "_type" "edge", "_outV" "4", "_inV" "3", "_label" "created"}
                  {"weight" 0.20000000298023224, "_id" "12", "_type" "edge", "_outV" "6", "_inV" "3", "_label" "created"}
                  {"_id" "21", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "new"}
                  {"weight" 1.0, "_id" "10", "_type" "edge", "_outV" "4", "_inV" "5", "_label" "created"}
                  {"_id" "1", "_type" "edge", "_outV" "3", "_inV" "0", "_label" "something"}
                  {"weight" 0.5, "_id" "7", "_type" "edge", "_outV" "1", "_inV" "2", "_label" "knows"}
                  {"weight" 0.4000000059604645, "_id" "9", "_type" "edge", "_outV" "1", "_inV" "3", "_label" "created"}
                  {"weight" 1.0, "_id" "8", "_type" "edge", "_outV" "1", "_inV" "4", "_label" "knows"}]}
        ))

  (facts "about graph-listener"
    (let [val (atom nil)]
      (.edgeAdded (graph-listener :edge-add (fn [e] (swap! val (fn [_] [:edge-added e])))) first-edge)
      @val => [:edge-added first-edge]

      (.edgePropertyChanged (graph-listener :edge-prop-changed (fn [e k v] (swap! val (fn [_] [:edge-prop-changed e k v])))) first-edge "foo" "bar")
      @val => [:edge-prop-changed first-edge "foo" "bar"]

      (.edgePropertyRemoved (graph-listener :edge-prop-remove (fn [e k v] (swap! val (fn [_] [:edge-prop-remove e k v])))) first-edge "bar" "foo")
      @val => [:edge-prop-remove first-edge "bar" "foo"]

      (.edgeRemoved (graph-listener :edge-remove (fn [e] (swap! val (fn [_] [:edge-remove e])))) first-edge)
      @val => [:edge-remove first-edge]

      (.vertexAdded (graph-listener :vertex-add (fn [v] (swap! val (fn [_] [:vertex-add v])))) first-vertice)
      @val => [:vertex-add first-vertice]

      (.vertexPropertyChanged (graph-listener :vertex-prop-changed (fn [vx k v] (swap! val (fn [_] [:vertex-prop-changed vx k v])))) first-vertice "bax" "quux")
      @val => [:vertex-prop-changed first-vertice "bax" "quux"]

      (.vertexPropertyRemoved (graph-listener :vertex-prop-remove (fn [vx k v] (swap! val (fn [_] [:vertex-prop-remove vx k v])))) first-vertice "bax2" "quux2")
      @val => [:vertex-prop-remove first-vertice "bax2" "quux2"]

      (.vertexRemoved (graph-listener :vertex-remove (fn [vx] (swap! val (fn [_] [:vertex-remove vx])))) first-vertice)
      @val => [:vertex-remove first-vertice]
      ))

  (facts "about event-graph"
    (event-graph g) => (instance-of EventGraph))

  (facts "about add-listener"
    (let [eg (event-graph g)]
      (add-listener eg {:edge-remove (fn [e] )})
      (add-listener eg (graph-listener :edge-remove (fn [e] )))
      (let [iter (.getListenerIterator eg)]
        (.next iter) (.next iter)
        (.hasNext iter) => false
      )))

  (facts "about tinker-graph"
    (tinker-graph) => (instance-of TinkerGraph))
)
