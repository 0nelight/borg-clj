(ns borgclj.serve-test
  "What a real borg serve does to a client."
  (:require [clojure.test :refer [deftest is testing]]
            [borgclj.bytes :as b]
            [borgclj.fixture :as fx]
            [borgclj.remote :as remote])
  (:import [java.nio.file Files Path]))

(def ^:const max-data-size
  "borg/constants.py: MAX_DATA_SIZE. A put above it is refused by the server,
   not by us -- so whatever --block is set to has to stay under it."
  20971479)

(defn- with-serve
  "Runs f with a connection to a real borg serve over a fresh repository."
  [f]
  (when-let [oracle (fx/oracle)]
    (let [dir (Files/createTempDirectory "borgclj-serve"
                                         (into-array java.nio.file.attribute.FileAttribute []))
          conn (remote/spawn [oracle "serve"])]
      (try
        (remote/open! conn (str (.resolve dir "repo")) :create true)
        (f conn)
        (finally
          (try (remote/close conn) (catch Exception _ nil))
          (doseq [^Path p (reverse (vec (iterator-seq (.iterator (Files/walk dir (into-array java.nio.file.FileVisitOption []))))))]
            (try (Files/deleteIfExists p) (catch Exception _ nil))))))))

(defn- id-of ^bytes [^long n]
  (let [a (byte-array 32)]
    (java.util.Arrays/fill a (unchecked-byte n))
    a))

(defn- inject! [conn ^String kind]
  (remote/call conn "inject_exception" {"kind" (b/unlatin1 kind)}))

(deftest every-error-class-arrives-as-itself
  (with-serve
    (fn [conn]
      ;; borg raises these through inject_exception with fixed arguments, so the
      (doseq [[kind want] [["DoesNotExist" "DoesNotExist"]
                           ["AlreadyExists" "AlreadyExists"]
                           ["CheckNeeded" "CheckNeeded"]
                           ["IntegrityError" "IntegrityError"]
                           ["PathNotAllowed" "PathNotAllowed"]
                           ["ObjectNotFound" "ObjectNotFound"]
                           ["InvalidRPCMethod" "InvalidRPCMethod"]
                           ;; anything the server did not plan for still has to
                           ["divide" "ZeroDivisionError"]]]
        (let [msg (try (inject! conn kind) nil
                       (catch clojure.lang.ExceptionInfo e (ex-message e)))]
          (is (some? msg) (str kind " kam ohne Ausnahme zurueck"))
          (is (re-find (re-pattern (str "remote: " want ":")) (str msg)) kind))))))

(deftest the-connection-survives-a-remote-error
  (with-serve
    (fn [conn]
      (try (inject! conn "IntegrityError") (catch clojure.lang.ExceptionInfo _ nil))
      (testing "a refused call leaves nothing half-read on the wire"
        (remote/put! conn (id-of 7) (byte-array [0 0 65]))
        (remote/drain-puts! conn)
        (is (= 3 (alength ^bytes (remote/get-object conn (id-of 7)))))))))

(deftest an-unknown-method-is-refused-not-ignored
  (with-serve
    (fn [conn]
      (let [msg (try (remote/call conn "__init__" {}) nil
                     (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (re-find #"remote: InvalidRPCMethod:" (str msg)))))))

(deftest a-missing-object-is-the-error-the-manifest-lookup-waits-for
  (with-serve
    (fn [conn]
      ;; create/manifest-plain treats exactly this message as "no manifest yet"
      (let [msg (try (remote/get-object conn (id-of 0)) nil
                     (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (re-find #"ObjectNotFound|DoesNotExist" (str msg)))))))

(deftest the-object-size-limit-belongs-to-the-server
  (with-serve
    (fn [conn]
      (testing "a chunk of exactly MAX_DATA_SIZE is stored"
        (remote/put! conn (id-of 1) (byte-array max-data-size))
        (remote/drain-puts! conn)
        (is (= max-data-size (alength ^bytes (remote/get-object conn (id-of 1))))))
      (testing "one byte more is refused, and we hear about it"
        (let [msg (try (remote/put! conn (id-of 2) (byte-array (inc max-data-size)))
                       (remote/drain-puts! conn)
                       nil
                       (catch clojure.lang.ExceptionInfo e (ex-message e)))]
          (is (some? msg) "der Server nahm einen zu grossen Chunk an")
          (is (re-find #"IntegrityError" (str msg))))))))
