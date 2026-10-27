(ns borgclj.core-test
  (:require [borgclj.archive :as arch]
            [borgclj.cache :as cache]
            [borgclj.create :as create]
            [borgclj.remote :as remote]
            [borgclj.store :as store]
            [clojure.string :as str]
            [borgclj.bytes :as b]
            [borgclj.chunker :as ch]
            [borgclj.compress :as compress]
            [borgclj.fixture :as fx]
            [borgclj.key :as key]
            [borgclj.msgpack :as mp]
            [borgclj.platform :as plat]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files Path]))

(def golden-chunks
  [["54c87eef64247d9d00c6849e24e10bf25e9792e7eeefe9057a874fdd1dcfbc29" 4568552]
   ["ef4775ec4b1c2e1633b268eddd9de950c86f3447984fe056c0a80226935f6bfa" 4729353]
   ["a8886d7d1f4766b6e529243da9a3a9e0c4b60f6dac0f8c7770cb31ad5b65ed9c" 2095805]
   ["1373a77d3b5106f9ba20b4308d7b2a0d78dafbad21590db4bbe1a262076bf8f9" 1189202]])

(def k-plain (key/plaintext-key))

(defn- tmpdir ^Path []
  (Files/createTempDirectory "borgclj-test" (into-array java.nio.file.attribute.FileAttribute [])))

(deftest msgpack-roundtrip
  (is (= {:a 1 :b [1 2 3]} (mp/unpack (byte-array (map unchecked-byte [0x82 0xa1 0x61 0x01 0xa1 0x62 0x93 0x01 0x02 0x03])))))
  (is (= -1 (mp/unpack (byte-array [(unchecked-byte 0xff)])))))

(deftest latin1-is-byte-faithful
  (let [raw (byte-array (map unchecked-byte [0x62 0x61 0x64 0xff 0xfe 0x6e]))]
    (is (java.util.Arrays/equals raw (b/unlatin1 (b/latin1 raw))))))

(deftest platform-seam
  (let [caps (plat/capabilities)]
    (is (contains? caps :xattr))
    (is (:process-handle caps))
    (is (:file-lock caps))
    (is (seq (:views caps)))))

(deftest chunker-matches-borg
  (let [dir (tmpdir)
        _ (fx/make-data! dir)
        mine (mapv (fn [c] [(b/hex (b/sha256 c)) (alength ^bytes c)])
                   (ch/chunks (.resolve dir "data/big.bin") 0))]
    (is (= golden-chunks mine))))

(deftest reads-a-real-borg-repo
  (if-let [_ (fx/oracle)]
    (let [dir (tmpdir)
          _ (fx/make-data! dir)
          rp (fx/make-repo! dir)
          r (store/local (str rp))
          k (key/plaintext-key)
          m (arch/manifest r k)
          a (arch/archive r k m "t1")
          its (arch/items r k a)
          by-path (into {} (map (juxt (comp b/display :path) identity) its))]
      (testing "manifest + item stream"
        (is (= ["t1"] (arch/archive-names m)))
        (is (= #{"data" "data/sub" "data/big.bin" "data/hello.txt" "data/sub/tiny.txt" "data/link"}
               (set (keys by-path)))))
      (testing "every object verifies against its chunk id"
        (doseq [id (store/list-ids r)
                :let [hx (b/hex id)]
                :when (not= hx (b/hex arch/manifest-id))]
          (is (bytes? (key/decrypt k id (store/get-object r id))))))
      (testing "chunker reproduces the ids borg stored"
        (is (= (mapv (fn [[id sz]] [(b/hex id) sz]) (:chunks (by-path "data/big.bin")))
               (mapv (fn [c] [(b/hex (b/sha256 c)) (alength ^bytes c)])
                     (ch/chunks (.resolve dir "data/big.bin") 0)))))
      (testing "extract is byte-identical"
        (let [out (.resolve dir "out")]
          (doseq [it its] (arch/extract-item! r k out it))
          (is (java.util.Arrays/equals (Files/readAllBytes (.resolve dir "data/big.bin"))
                                       (Files/readAllBytes (.resolve out "data/big.bin"))))
          (is (= "hallo borg\n" (slurp (.toFile (.resolve out "data/hello.txt"))))))))
    (println "SKIP reads-a-real-borg-repo: no oracle (set BORG_ORACLE)")))

(deftest writes-to-a-real-borg-serve
  (if-let [o (fx/oracle)]
    (let [dir (tmpdir)
          _ (fx/make-data! dir)
          repo (str (.resolve dir "repo"))
          conn (remote/spawn [o "serve" "--restrict-to-path" (str dir)])
          k (key/plaintext-key)]
      (try
        (remote/open! conn repo :create true)
        (let [res (create/create! conn k "a1" [(.resolve dir "data")])]
          (is (= 3 (:nfiles res)))
          (is (= 6 (:items res))))
        (finally (remote/close conn)))
      (let [conn2 (remote/spawn [o "serve" "--restrict-to-path" (str dir)])]
        (try
          (remote/open! conn2 repo)
          (create/create! conn2 k "a2" [(.resolve dir "data/sub")])
          (finally (remote/close conn2))))
      (testing "the server's own borg reads and checks what we wrote"
        (let [run (fn [& args]
                    (let [pb (ProcessBuilder. ^java.util.List (vec (cons o args)))]
                      (.putAll (.environment pb) {"BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK" "yes"})
                      (.redirectErrorStream pb true)
                      (let [p (.start pb) out (slurp (.getInputStream p))]
                        [(.waitFor p) out])))
              [rc-list out-list] (run "list" "--short" (str repo "::a1"))
              [rc-check out-check] (run "check" "--verify-data" "-v" repo)]
          (is (zero? rc-list))
          (is (= #{"data" "data/big.bin" "data/hello.txt" "data/link" "data/sub" "data/sub/tiny.txt"}
                 (set (remove str/blank? (str/split-lines (str/replace out-list #"(?m)^(Warning|Do you).*$" ""))))))
          (is (zero? rc-check) out-check)
          (is (re-find #"no problems found" out-check)))))
    (println "SKIP writes-to-a-real-borg-serve: no oracle")))

(deftest location-parsing
  (is (= {:argv ["ssh" "user@host" "borg" "serve"] :path "/srv/borg/repo"}
         (update (remote/location->argv "ssh://user@host/srv/borg/repo" {}) :argv vec)))
  (is (= {:argv ["ssh" "user@host" "borg" "serve"] :path "backup/repo"}
         (update (remote/location->argv "user@host:backup/repo" {}) :argv vec)))
  (is (= ["plink" "-i" "key.ppk" "user@host" "borg" "serve"]
         (vec (:argv (remote/location->argv "ssh://user@host/repo" {:rsh "plink -i key.ppk"})))))
  (is (nil? (remote/location->argv "/local/repo" {}))))

(deftest remote-path-option
  (is (= ["ssh" "u@h" "/opt/borg" "serve"]
         (vec (:argv (remote/location->argv "ssh://u@h/repo" {:remote-path "/opt/borg"}))))))

(deftest windows-items-never-carry-empty-owner
  (let [dir (tmpdir)
        _ (fx/make-data! dir)
        it (create/item-for (.resolve dir "data") (.resolve dir "data/hello.txt"))]
    (is (not (str/blank? (:user it))))
    (is (not (str/blank? (:group it))))
    (is (pos? (long (:mode it))))
    (is (= "data/hello.txt" (:path it)))))

(deftest dos-attributes-become-a-posix-mode
  (is (= 0100444 (create/fallback-mode {"readonly" true} :file)))
  (is (= 0100644 (create/fallback-mode {"readonly" false} :file)))
  (is (= 040755 (create/fallback-mode {} :dir)))
  (is (= 0120777 (create/fallback-mode {} :symlink))))

(deftest cache-roundtrip-keeps-slashes-in-paths
  (let [tmp (str (tmpdir))
        _ (System/setProperty "x" "x")
        repo-id (b/hex (byte-array 32))
        c (-> (cache/empty-cache repo-id)
              (cache/remember "data/sub/deep file.txt"
                              {:size 10 :mtime 123 :ino 7}
                              [[(b/unhex (apply str (repeat 64 "a"))) 10 12]])
              (assoc :chunks #{(apply str (repeat 64 "a"))} :manifest-id "beef"))]
    (with-redefs [cache/cache-root (fn [] (Path/of tmp (into-array String [])))]
      (cache/save-cache! c)
      (let [back (cache/load-cache repo-id)]
        (is (= "beef" (:manifest-id back)))
        (is (= #{"data/sub/deep file.txt"} (set (keys (:files back)))))
        (is (= (:chunks c) (:chunks back)))
        (is (some? (cache/hit back "data/sub/deep file.txt" {:size 10 :mtime 123 :ino 7})))))))

(deftest cache-misses-on-any-change
  (let [id (apply str (repeat 64 "b"))
        c (-> (cache/empty-cache "r")
              (cache/remember "f" {:size 10 :mtime 123 :ino 7} [[(b/unhex id) 10 12]])
              (assoc :chunks #{id}))]
    (is (some? (cache/hit c "f" {:size 10 :mtime 123 :ino 7})))
    (is (nil? (cache/hit c "f" {:size 11 :mtime 123 :ino 7})) "size")
    (is (nil? (cache/hit c "f" {:size 10 :mtime 124 :ino 7})) "mtime")
    (is (nil? (cache/hit c "f" {:size 10 :mtime 123 :ino 8})) "inode")
    (is (nil? (cache/hit c "other" {:size 10 :mtime 123 :ino 7})) "path")
    (testing "a chunk the repository does not have is never reused"
      (is (nil? (cache/hit (assoc c :chunks #{}) "f" {:size 10 :mtime 123 :ino 7}))))))

(deftest cache-expires-unseen-entries
  (let [c (-> (cache/empty-cache "r")
              (cache/remember "a" {:size 1 :mtime 1 :ino 1} [])
              (cache/remember "b" {:size 1 :mtime 1 :ino 1} []))
        after (reduce (fn [acc _] (cache/expire acc #{"a"})) c (range (inc cache/ttl)))]
    (is (= #{"a"} (set (keys (:files after)))))))

(deftest second-run-reuses-every-file
  (if-let [o (fx/oracle)]
    (let [dir (tmpdir)
          _ (fx/make-data! dir)
          repo (str (.resolve dir "repo"))
          cdir (str (.resolve dir "cache"))
          run (fn [archive create?]
                (let [conn (remote/spawn [o "serve" "--restrict-to-path" (str dir)])]
                  (try
                    (let [rid (b/hex (remote/open! conn repo :create create?))]
                      (with-redefs [cache/cache-root (fn [] (Path/of cdir (into-array String [])))]
                        (create/create! conn k-plain archive [(.resolve dir "data")] rid)))
                    (finally (remote/close conn)))))
          r1 (run "c1" true)
          r2 (run "c2" false)]
      (is (= 3 (:read r1)))
      (is (= 0 (:reused r1)))
      (is (= 0 (:read r2)) "second run reads nothing from disk")
      (is (= 3 (:reused r2)))
      (is (= (:size r1) (:size r2))))
    (println "SKIP second-run-reuses-every-file: no oracle")))

(deftest restores-over-the-wire
  (if-let [o (fx/oracle)]
    (let [dir (tmpdir)
          _ (fx/make-data! dir)
          rp (fx/make-repo! dir)
          conn (remote/spawn [o "serve" "--restrict-to-path" (str dir)])
          st (store/remote conn (str rp))
          out (.resolve dir "restored")]
      (try
        (let [m (arch/manifest st k-plain)
              a (arch/archive st k-plain m "t1")
              its (arch/items st k-plain a)]
          (is (= ["t1"] (arch/archive-names m)))
          (is (= 6 (count its)))
          (doseq [it its] (arch/extract-item! st k-plain out it))
          (is (java.util.Arrays/equals (Files/readAllBytes (.resolve dir "data/big.bin"))
                                       (Files/readAllBytes (.resolve out "data/big.bin"))))
          (is (= "hallo borg\n" (slurp (.toFile (.resolve out "data/hello.txt"))))))
        (finally (store/close st))))
    (println "SKIP restores-over-the-wire: no oracle")))

(deftest local-store-reads-payloads-on-demand
  (if-let [o (fx/oracle)]
    (let [dir (tmpdir)
          _ (fx/make-data! dir)
          rp (fx/make-repo! dir)
          st (store/local (str rp))
          d (store/describe st)]
      (is (= :local (:kind d)))
      (is (pos? (long (:objects d))))
      (is (bytes? (store/get-object st (byte-array 32))))
      (is (= 6 (count (arch/items st k-plain (arch/archive st k-plain (arch/manifest st k-plain) "t1"))))))
    (println "SKIP local-store-reads-payloads-on-demand: no oracle")))

(deftest compression-specs-follow-borgs-own-bounds
  (testing "what borg accepts, we accept -- with borg's defaults"
    (is (= {:kind :none} (compress/parse-spec nil)))
    (is (= {:kind :none} (compress/parse-spec "")))
    (is (= {:kind :none} (compress/parse-spec "none")))
    (is (= {:kind :lz4} (compress/parse-spec "lz4")))
    (is (= {:kind :zstd :level 3} (compress/parse-spec "zstd")))
    (is (= {:kind :zstd :level 1} (compress/parse-spec "zstd,1")))
    (is (= {:kind :zstd :level 22} (compress/parse-spec "zstd,22"))))
  (testing "a level outside zstd's range never reaches zstd"
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "zstd,0")))
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "zstd,23"))))
  (testing "a level that is not a number, or missing after the comma, is not silently a three"
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "zstd,abc")))
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "zstd,"))))
  (testing "a level handed to a compressor that has none used to be dropped in silence"
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "lz4,9")))
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "none,1"))))
  (testing "what we can read but not write says so, and the rest is simply unknown"
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "zlib,6")))
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "lzma")))
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "brotli")))
    (is (thrown? clojure.lang.ExceptionInfo (compress/parse-spec "zstd,3,4")))))
