(ns neo4j-clj.core
  (:use clojure.core)
  (:import [org.neo4j.kernel EmbeddedGraphDatabase]
           [org.neo4j.graphdb Direction NotFoundException RelationshipType]
           [org.neo4j.cypher.javacompat ExecutionEngine CypherParser]))

;;; --------------------------------------------------------------------------------

(def ^:dynamic *db* nil)

(defn make-embedded-db
  [path]
  (EmbeddedGraphDatabase. path))

(defn set-db!
  [db]
  (alter-var-root #'*db* (constantly db)))

(defn shutdown
  ([]
     (.shutdown *db*))
  ([db]
     (.shutdown db)))

(defn reference-node
  ([]
     (reference-node *db*))
  ([db]
     (.getReferenceNode db)))

(defn get-all-nodes
  ([]
     (get-all-nodes *db*))
  ([db]
     (.getAllNodes db)))

(defn get-node-by-id
  ([node-id]
     (get-node-by-id *db* node-id))
  ([db node-id]
  (.getNodeById db node-id)))

(defn get-id
  [node-or-relationship]
  (.getId node-or-relationship))

(defn ^RelationshipType as-relationship
  "Creates a Neo4j relationship type object from a keyword.

  Copied from borneo hence same caveats apply: should we cache these?
  "
  [k]
  (reify RelationshipType
         (^String name [this] (name k))))

(defmacro with-connection [db & body]
  `(binding [*db* ~db]
     ~@body))

(defmacro with-transaction
  [& body]
  `(let [transaction# (.beginTx *db*)]
     (try
       (let [val# (do ~@body)]
         (.success transaction#)
         val#)
       (finally (.finish transaction#)))))

(defmacro with-transaction*
  [db & body]
  `(with-connection ~db
     (with-transaction
       ~@body)))

;;; --------------------------------------------------------------------------------
;;  Property Containers

(defn associate
  ([container key val]
     (.setProperty container (name key) val)
     container)
  ([container m]
     (doseq [[k v] m]
       (associate container k v))
     container))

(defn disassociate
  ([container k-or-ks]
     (if (keyword? k-or-ks)
       (.removeProperty container (name k-or-ks))
       (doseq [k k-or-ks]
         (disassociate container k)))
     container))

(defn property
  [container k]
  (try
    (.getProperty container (name k))
    (catch NotFoundException e
      nil)))

(defn properties
  ([container]
     (properties container (.getPropertyKeys container)))
  ([container ks]
     (doall
      (reduce (fn [m k]
                (assoc m (keyword k) (property container k)))
              {}
              ks))))

;;; --------------------------------------------------------------------------------
;;  Relationships

(defn ^Direction as-direction
  "Translates keyword to the respective relationship direction.
  Valid values are :out :in and :both.
  See javadoc for Direction for more info on directions."
  [k]
  (case k
        :out  Direction/OUTGOING
        :in   Direction/INCOMING
        :both Direction/BOTH))

(defn relate
  ([n1 n2 via]
     (.createRelationshipTo n1 n2 (as-relationship via)))
  ([n1 n2 via m]
     (doto (.createRelationshipTo n1 n2 (as-relationship via))
       (associate m))))

(defn relationship-type
  [rel]
  (keyword (.. rel getType name)))

(defn relationship-type?
  [rel type]
  (= (relationship-type rel) type))

(defn start-node
  [rel]
  (.getStartNode rel))

(defn end-node
  [rel]
  (.getEndNode rel))

(defn other-node
  [rel node]
  (.getOtherNode rel node))

(defn nodes
  [rel]
  (seq (.getNodes rel)))

(defn delete
  [rel]
  (.delete rel))

;;; --------------------------------------------------------------------------------
;;  Nodes

(defn create-node
  ([]
     (.createNode *db*))

  ([m]
     (doto (create-node)
       (associate m))))

(defn relationships
  ([node]
     (.getRelationships node))

  ([node direction]
     (.getRelationships node (as-direction direction))))

(defn relationship
  ([node kind]
     (relationship node kind :both))

  ([node kind direction]
     (.getSingleRelationship node (as-relationship kind) (as-direction direction))))

(defn relationship?
  ([node]
     (.hasRelationship node))

  ([node direction]
     (.hasRelationship node (as-direction direction)))

  ([node direction kind-or-kinds]
     (if (keyword? kind-or-kinds)
       (.hasRelationship node (as-relationship kind-or-kinds) (as-direction direction))
       (.hasRelationship node
                         (as-direction direction)
                         (into-array RelationshipType (map as-relationship kind-or-kinds))))))

;;; --------------------------------------------------------------------------------
;;  Indexes

(defn create-node-index
  [index-name]
  (let [index-manager (.index *db*)]
    (.forNodes index-manager (name index-name))))

(defn create-relationship-index
  [index-name]
  (let [index-manager (.index *db*)]
    (.forRelationships index-manager (name index-name))))

(defn add-to-index
  [index node k v]
  (.add index node (name k) v)
  node)

(defn- search-node-index
  [index]
  (if (or (instance? String index) (instance? clojure.lang.Keyword index))
    (create-node-index index)
    index))

(defn- search-relationship-index
  [index]
  (if (or (instance? String index) (instance? clojure.lang.Keyword index))
    (create-relationship-index index)
    index))

(defn delete-node-index
  [index]
  (let [index (search-node-index index)]
    (.delete index)))

(defn delete-relationship-index
  [index]
  (let [index (search-relationship-index)]
    (.delete index)))

(defn- remove-from-index
   ([index node]
      (.remove index node))
   ([index node k]
      (.remove index k))
   ([index node k v]
      (.remove index k v)))

(defn remove-from-node-index
  [index & args]
  (let [index (search-node-index index)]
    (apply remove-from-index index args)))

(defn remove-from-relationship-index
  [index & args]
  (let [index (search-relationship-index index)]
    (apply remove-from-index index args)))

(defn- get-hits
  [results]
  (seq results))

(defn- get-hit
  [results]
  (.getSingle results))

(defn- get-relationship-results
  [index k v]
  (let [index (search-relationship-index index)]
    (.get index (name k) v)))

(defn get-relationship-hits
  [& args]
  (get-hits (apply get-relationship-results args)))

(defn get-relationship-hit
  [& args]
  (get-hit (apply get-relationship-results args)))

(defn- query-relationship-results
  ([index q]
     (query-relationship-results index q nil nil))
  ([index k q]
     (query-relationship-results index k q nil nil))
  ([index q start-node end-node]
     (let [index (search-relationship-index index)]
       (.query index q start-node end-node)))
  ([index k q start-node end-node]
     (let [index (search-relationship-index index)]
       (.query index (name k) q start-node end-node))))

(defn query-relationship-hits
  [& args]
  (get-hits (apply query-relationship-results args)))

(defn query-relationship-hit
  [& args]
  (get-hit (apply query-relationship-results args)))

(defn- get-node-results
  [index k v]
  (let [index (search-node-index index)]
    (.get index (name k) v)))

(defn get-node-hits
  [& args]
  (get-hits (apply get-node-results args)))

(defn get-node-hit
  [& args]
  (get-hit (apply get-node-results args)))

(defn- query-node-results
  [index & args]
  (let [index (search-node-index index)
        function (fn
                   ([index q]
                      (.query index q))
                   ([index k q]
                      (.query index (name k) q)))]
    (apply function index args)))

(defn query-node-hits
  [& args]
  (get-hits (apply query-node-results args)))

(defn query-node-hit
  [& args]
  (get-hit (apply query-node-results args)))

;;; --------------------------------------------------------------------------------
;;  Cypher Interface

;; db = new ImpermanentGraphDatabase();
;; CypherParser parser = new CypherParser();
;; ExecutionEngine engine = new ExecutionEngine(db);
;; Query query = parser.parse( "start n=(0) where 1=1 return n" );
;; ExecutionResult result = engine.execute( query );

;; assertThat( result.columns(), hasItem( "n" ) );
;; Iterator<Node> n_column = result.columnAs( "n" );
;; assertThat( asIterable( n_column ), hasItem(db.getNodeById(0)) );
;; assertThat( result.toString(), containsString("Node[0]"

(defn cypher-query
  ([query]
     (cypher-query *db* query))

  ([db query]
     (let [engine (ExecutionEngine. db)
           parser (CypherParser.)]
       (.parse parser query)
       (let [result (.execute engine (.parse parser query))]
         (with-meta
           (seq result)
           {:columns (.columns result)})))))

(defn result-columns
  [r]
  (:columns (meta r)))

(defn cypher-result-as-rows
  [results]
  (let [cols (result-columns results)]
    (with-meta
      (map (fn [row]
             (map #(.get row %) cols))
           results)
      (meta results))))

(defn cypher-result-as-nodes
  "Lots of queries return a single column in the result set, in that
  case it is a pain to deal with a seq of seqs of only one item.

  This flattens out the results to give you just a seq of nodes.

  If the result has more than one column the rest are silently ignored."
  [result]
  (let [col (first (result-columns result))]
    (with-meta
      (map (fn [row] (.get row col))
           result)
      {:columns [col]})))

;;; --------------------------------------------------------------------------------
;;  Some nice to have utilities

(defn follow
  [node relationship-type]
  (let [rel (relationship node relationship-type)]
    (.getOtherNode rel node)))
