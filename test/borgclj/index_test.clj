(ns borgclj.index-test
  (:require [borgclj.bytes :as b]
            [borgclj.fixture :as fx]
            [borgclj.repository :as repo]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files Path]))

(defn- tmpdir ^Path []
  (Files/createTempDirectory "borgclj-idx" (into-array java.nio.file.attribute.FileAttribute [])))

(deftest borg-index-agrees-with-a-walk-over-the-segments
  (if-not (fx/oracle)
    (println "  (no borg oracle on this host — skipping the index cross-check)")
    (let [dir (tmpdir)]
      (try
        (fx/make-data! dir)
        (fx/make-repo! dir)
        (let [rp (.resolve dir "repo")
              walked (repo/index rp)
              from-index (repo/index-from-hashindex rp)]
          (is (seq walked))
          (is (seq from-index) "borg leaves an index file behind after a commit")
          (is (= (set (keys walked)) (set (keys from-index)))
              "the index names exactly the objects the segments hold")
          (testing "and it points at the same bytes"
            (doseq [hx (take 20 (keys walked))]
              (is (= (seq (repo/read-object (get walked hx)))
                     (seq (repo/read-object (get from-index hx))))
                  hx))))
        (finally
          (doseq [^Path p (reverse (vec (iterator-seq (.iterator (Files/walk dir (make-array java.nio.file.FileVisitOption 0))))))]
            (Files/deleteIfExists p)))))))
