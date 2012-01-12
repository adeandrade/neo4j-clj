(ns neo4j-clj.test.core
  (:use [clojure.test]
        [clojure.pprint :only [pprint]]
        [neo4j-clj.test.test-utils :only [with-temp-dir with-shutdown]])
  (:use neo4j-clj.core :reload))

(deftest test-basic-property-usage
  (with-temp-dir db-dir
    (let [db (make-embedded-db (.getAbsolutePath db-dir))]
      (with-shutdown db
        (with-transaction* db
          ;; properties on nodes
          (let [n1   (create-node)
                n2   (create-node)]
            (associate n1 :name "name")
            (associate n1 "name2" "name2")
            (associate n1 {:value1 "value1"
                           :value2 "value2"})
            (is (= (property n1 :name)   "name"))
            (is (= (property n1 :name2)  "name2"))
            (is (= (property n1 "name2") "name2"))

            (is (= (properties n1)
                   {:name   "name"
                    :name2  "name2"
                    :value1 "value1"
                    :value2 "value2"}))

            (disassociate n1 [:value1 :value2])
            (is (= (properties n1)
                   {:name   "name"
                    :name2  "name2"})))

          ;; properties on relationships
          (let [n1 (create-node {:1 "one"
                                 :2 "two"
                                 :3 3})
                n2 (create-node)
                r1 (relate n1 n2 :knows {:1 "one"
                                         :2 "two"
                                         :3 3})]
            (is (= (properties n1)
                   {:1 "one"
                    :2 "two"
                    :3 3}))
            (is (= (properties r1)
                   {:1 "one"
                    :2 "two"
                    :3 3}))

            (disassociate r1 [:1 :3])
            (is (= (properties r1) {:2 "two"}))))))))


(deftest test-basic-relationship-usage
  (with-temp-dir db-dir
    (let [connection (make-embedded-db (.getAbsolutePath db-dir))]
      (with-shutdown connection
        (with-transaction* connection
          (let [n1   (create-node)
                n2   (create-node)
                rel1 (relate n1 n2 :knows)]

            (is (= (map relationship-type (relationships n1))
                   (map relationship-type (relationships n2))
                   [:knows]))

            (let [rel (relate n2 n1 :kinda-knows)]
              (is (= (map relationship-type (relationships n1))
                     (map relationship-type (relationships n2))
                     [:knows :kinda-knows]))

              (do
                (is (= (map relationship-type (relationships n1 :out))
                       [:knows]))

                (is (= (map relationship-type (relationships n1 :in))
                       [:kinda-knows]))

                (is (= (map relationship-type (relationships n1 :both))
                       [:knows :kinda-knows]))

                (is (relationship-type (relationship n1 :knows)))
                (is (relationship-type (relationship n1 :knows :out)))
                (is (not (relationship n1 :knows :in)))
                (is (not (relationship n1 :some-other-type))))

              (do
                (is (relationship? n1))
                (is (relationship? n1 :out))
                (is (relationship? n1 :out :knows))
                (is (relationship? n1 :out [:knows])))

              (do
                (is (relationship-type? rel :kinda-knows))
                (is (not (relationship-type? rel :some-other-type)))
                (is (= (start-node rel) n2))
                (is (= (end-node   rel) n1))
                (is (= (other-node rel n1) n2))
                (is (= (other-node rel n2) n1))
                (is (= (nodes rel) [n2 n1]))

                (associate rel {:value1 "value1"
                                :value2 "value2"})
                (is (= (properties rel)
                       {:value1 "value1"
                        :value2 "value2"})))

              (do
                (delete rel)
                (is (= (map relationship-type (relationships n1))
                       (map relationship-type (relationships n2))
                       [:knows]))))))))))

(deftest test-basic-index-usage
  (with-temp-dir db-dir
    (let [connection (make-embedded-db (.getAbsolutePath db-dir))]
      (with-shutdown connection
        (with-transaction* connection
          (let [n1   (create-node)
                n2   (create-node)
                rel1 (relate n1 n2 :knows)]

            (let [idx (create-node-index :index)]
              (add-to-index idx n1 :key "one")
              (add-to-index idx n1 :key "1")
              (add-to-index idx n2 :key "two")
              (is (= n1 (get-hit idx :key "one")))
              (is (= n1 (get-hit idx :key "1")))

              (is (= n2 (get-hit idx :key "two"))))

            (let [idx (create-relationship-index :index)]
              (add-to-index idx rel1 :key "one")
              (add-to-index idx rel1 :key "1")

              (is (= rel1 (get-hit idx :key "one")))
              (is (= rel1 (get-hit idx :key "1"))))))))))

(deftest test-basic-follow-usage
  (with-temp-dir db-dir
    (let [connection (make-embedded-db (.getAbsolutePath db-dir))]
      (with-shutdown connection
        (with-transaction* connection
          (let [n1   (create-node)
                n2   (create-node)
                rel1 (relate n1 n2 :knows)]

            (is (= (follow n1 :knows) n2))))))))

(deftest test-cypher-query
  (with-temp-dir db-dir
    (let [connection (make-embedded-db (.getAbsolutePath db-dir))]
      (with-shutdown connection
        (with-connection connection
          (let [result (cypher-query "start n=node(0) where 1=1 return n")
                rows   (cypher-result-as-rows  result)
                nodes  (cypher-result-as-nodes result)]
            (is (= (count rows) (count nodes)))

            (is (= ["n"] (result-columns result)))
            (is (= ["n"] (result-columns rows)))
            (is (= ["n"] (result-columns nodes)))

            (is (some #{(reference-node)} (first rows))))

          (let [result (cypher-query "start n=node(0), m=node(0) where 1=1 return n,m")
                rows   (cypher-result-as-rows  result)
                nodes  (cypher-result-as-nodes result)]
            (is (= (count rows) (count nodes)))

            (is (= ["n", "m"] (result-columns result)))
            (is (= ["n", "m"] (:columns (meta rows))))
            (is (= ["n"]      (:columns (meta nodes))))))))))
