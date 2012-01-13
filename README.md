# neo4j-clj

A clojure library that provides a more clojuresque way of interacting
with the Neo4j.

Tests against Neo4j `EmbeddedGraphDatabase` and `RestGraphDatabase`
from https://github.com/neo4j/java-rest-binding.

## Usage

```clojure
(def db (make-embedded-db "/tmp/neo4j"))

;; or (def db (RestGraphDatabase. "http://localhost:7474/db/data"))

(def n1 (with-transaction* db (create-node)))
(def n2 (with-transaction* db (create-node)))

(with-transaction* db (associate n1 :name "name"))
=> #<NodeProxy Node[1]>

(property n1 :name)
=> "name"

(property n1 "name")
=> "name"

(with-transaction* db (associate n1 :value "value"))
=> #<NodeProxy Node[1]>

(properties n1)
=> {:value "value", :name "name"}

(with-transaction* db (disassociate n1 [:value]))
=> #<NodeProxy Node[1]>

(properties n1)
=> {:name "name"}

(def r1 (with-transaction* db (relate n1 n2 :knows {:1 "one"
                                                    :2 "two"
                                                    :3 3})))
(properties r1)
=> {:2 "two", :3 3, :1 "one"}

(follow n1 :knows)
=> #<NodeProxy Node[2]>

(def r (with-transaction* db (cypher-query "start n=node(0) where 1=1 return n")))

(cypher-result-as-rows  r)
=> ((#<NodeProxy Node[0]>))

(cypher-result-as-nodes  r)
=> (#<NodeProxy Node[0]>)

(.shutdown db)
```

## Testing

In order to run the tests you need to add the plugin available from
https://github.com/jexp/neo4j-clean-remote-db-addon.

## Installation

`neo4j-clj` is available as a Maven artifact from
[Clojars](http://clojars.org/neo4j-clj):

```clojure
[neo4j-clj "0.0.1-SNAPSHOT"]
```

## License

Released under the MIT License:
<http://www.opensource.org/licenses/mit-license.php>
