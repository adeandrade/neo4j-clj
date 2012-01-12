(ns neo4j-clj.test.test-utils
  (:import [org.apache.commons.io FileUtils]))

(defmacro with-temp-dir [dirvar & body]
  `(let [tmpdir# (doto (java.io.File/createTempFile "neo4j-clj" "tmp")
                   (.delete)
                   (.mkdir))
         ~dirvar tmpdir#]
     (try
       ~@body
       (finally (FileUtils/deleteDirectory tmpdir#)))))

(defmacro with-shutdown [o & body]
  `(try
     ~@body
     (finally (.shutdown ~o))))
