(ns borgclj.fixture
  (:require [clojure.java.io :as io]
            [clojure.string])
  (:import [java.nio.file Files Path]
           [java.util Random]))

(defn path ^Path [^String s] (Path/of s (into-array String [])))

(defn make-data! [^Path dir]
  (Files/createDirectories (.resolve dir "data/sub") (into-array java.nio.file.attribute.FileAttribute []))
  (let [r (Random. 42)
        buf (byte-array 65536)]
    (with-open [os (io/output-stream (.toFile (.resolve dir "data/big.bin")))]
      (dotimes [_ 192]
        (.nextBytes r buf)
        (.write os buf))))
  (spit (.toFile (.resolve dir "data/hello.txt")) "hallo borg\n")
  (spit (.toFile (.resolve dir "data/sub/tiny.txt")) "xxxxxxxxxx")
  (let [link (.resolve dir "data/link")]
    (when-not (Files/exists link (into-array java.nio.file.LinkOption [java.nio.file.LinkOption/NOFOLLOW_LINKS]))
      (Files/createSymbolicLink link (path "hello.txt") (into-array java.nio.file.attribute.FileAttribute []))))
  dir)

(defn oracle []
  (if-let [p (System/getenv "BORG_ORACLE")]
    (when (Files/isExecutable (path p)) p)
    (->> (clojure.string/split (or (System/getenv "PATH") "")
                               (re-pattern (java.util.regex.Pattern/quote
                                            (System/getProperty "path.separator"))))
         (map #(str % "/borg"))
         (filter #(Files/isExecutable (path %)))
         first)))

(defn make-repo! [^Path dir]
  (when-let [o (oracle)]
    (let [env {"BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK" "yes"
               "BORG_RELOCATED_REPO_ACCESS_IS_OK" "yes"}
          run (fn [& args]
                (let [pb (ProcessBuilder. ^java.util.List (vec (cons o args)))]
                  (.putAll (.environment pb) env)
                  (.directory pb (.toFile dir))
                  (.redirectErrorStream pb true)
                  (let [p (.start pb) out (slurp (.getInputStream p))]
                    (when-not (zero? (.waitFor p)) (throw (ex-info "oracle failed" {:args args :out out})))
                    out)))]
      (run "init" "--encryption=none" "repo")
      (run "create" "--compression=none" "repo::t1" "data")
      (.resolve dir "repo"))))
