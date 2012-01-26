(defproject neo4j-clj "0.0.2-SNAPSHOT"
  :description      "Simple Clojuresque wrapper for Neo4J."
  :dependencies     [[org.clojure/clojure "1.3.0"]
                     [org.neo4j/neo4j "1.6"]]
  :dev-dependencies [[commons-io/commons-io "2.0.1"]
                     [swank-clojure "1.4.0-SNAPSHOT"]
                     ;;
                     [org.neo4j/neo4j-rest-graphdb "1.6"]
                     [clj-http "0.2.7"]]
  :repositories     {"neo4j-releases" "http://m2.neo4j.org/content/repositories/releases/"})
