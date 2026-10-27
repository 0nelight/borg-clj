(ns borgclj.image-test
  (:require [borgclj.archive :as arch]
            [clojure.string :as str]
            [borgclj.bytes :as b]
            [borgclj.create :as create]
            [borgclj.compress :as compress]
            [borgclj.fixture :as fx]
            [borgclj.image :as image]
            [borgclj.key :as key]
            [borgclj.remote :as remote]
            [borgclj.restore :as restore]
            [borgclj.source :as src]
            [borgclj.store :as store]
            [borgclj.cli :as cli]
            [borgclj.win.volume :as wv]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files Path StandardOpenOption]
           [java.util Arrays]))

(def ^:const mib (* 1024 1024))

(def ^:private made (atom []))

(defn- tmpdir ^Path []
  (let [d (Files/createTempDirectory "borgclj-image" (into-array java.nio.file.attribute.FileAttribute []))]
    (swap! made conj d)
    d))

(defn- delete-tree! [^Path root]
  (doseq [^Path p (reverse (vec (iterator-seq (.iterator (Files/walk root (into-array java.nio.file.FileVisitOption []))))))]
    (try (Files/deleteIfExists p) (catch Exception _ nil))))

(clojure.test/use-fixtures :once
  (fn [t] (try (t) (finally (doseq [d @made] (delete-tree! d)) (reset! made [])))))

(defn- write! [^Path f ^bytes d]
  (Files/write f d (into-array StandardOpenOption
                               [StandardOpenOption/CREATE StandardOpenOption/TRUNCATE_EXISTING
                                StandardOpenOption/WRITE])))

(deftest normalize-merges-and-aligns
  (is (= [[0 1024]] (src/normalize [[0 100] [100 400] [500 300]] 4096 512)))
  (is (= [[0 512] [4096 512]] (src/normalize [[10 1] [4100 1]] 8192 512)))
  (is (= [[0 4096]] (src/normalize [[0 99999]] 4096 512)))
  (is (= [] (src/normalize [[8192 100]] 4096 512))))

(deftest covering-clips-to-the-block
  (let [idx (src/index [[0 100] [1000 100] [5000 100]])]
    (is (= [[0 100]] (src/covering idx 0 500)))
    (is (= [[1000 100]] (src/covering idx 500 1000)))
    (is (= [[0 100] [1000 100]] (src/covering idx 0 2000)))
    (is (= [] (src/covering idx 2000 3000)))
    (is (= [[1050 50]] (src/covering idx 1050 100)))))

(deftest runs-become-byte-extents
  (is (= [[0 4096] [40960 8192]] (src/runs->extents [[0 1] [10 2]] 4096))))

(deftest free-space-reads-nothing-and-becomes-zeros
  (let [dir (tmpdir)
        f (.resolve dir "disk.raw")
        raw (byte-array (* 16 mib))
        _ (.nextBytes (java.util.Random. 1) raw)
        _ (write! f raw)
        used [[0 (* 4 mib)] [(* 8 mib) (* 4 mib)]]
        s (src/file-source f used)
        p (image/plan s (* 4 mib))
        buf (byte-array (* 4 mib))]
    (try
      (is (= 4 (:count p)))
      (is (= (* 8 mib) (:allocated p)))
      (is (= [:full (* 4 mib)] (image/read-block! s p buf 0 (* 4 mib))))
      (is (= [:zero 0] (image/read-block! s p buf (* 4 mib) (* 4 mib))))
      (is (image/zero-block? buf (* 4 mib)))
      (is (= [:full (* 4 mib)] (image/read-block! s p buf (* 8 mib) (* 4 mib))))
      (is (= [:zero 0] (image/read-block! s p buf (* 12 mib) (* 4 mib))))
      (finally (src/s-close s)))))

(deftest partial-block-zero-fills-the-gap
  (let [dir (tmpdir)
        f (.resolve dir "d.raw")
        raw (byte-array 8192)]
    (Arrays/fill raw (byte 0x41))
    (write! f raw)
    (let [s (src/file-source f [[0 512] [4096 512]])
          p (image/plan s 8192)
          buf (byte-array 8192)]
      (try
        (is (= [:partial 1024] (image/read-block! s p buf 0 8192)))
        (is (= 0x41 (bit-and (aget buf 0) 0xff)))
        (is (zero? (aget buf 600)))
        (is (= 0x41 (bit-and (aget buf 4096) 0xff)))
        (is (zero? (aget buf 5000)))
        (finally (src/s-close s))))))

(deftest composite-reads-span-parts
  (let [dir (tmpdir)
        a (.resolve dir "a.bin") bb (.resolve dir "b.bin")
        da (byte-array 4096) db (byte-array 4096)]
    (Arrays/fill da (byte 1)) (Arrays/fill db (byte 2))
    (write! a da) (write! bb db)
    (let [sa (src/file-source a) sb (src/file-source bb)
          parts [{:base 0 :size 4096 :inner 0 :src sa :own true}
                 {:base 4096 :size 4096 :inner 0 :src sb :own true}]
          c (src/->CompositeSource "c" 8192 512 [[0 8192]] [] parts {})
          out (byte-array 8192)]
      (try
        (src/s-read c 0 out 8192)
        (is (= 1 (aget out 4095)))
        (is (= 2 (aget out 4096)))
        (let [mid (byte-array 1024)]
          (src/s-read c 3584 mid 1024)
          (is (= 1 (aget mid 511)))
          (is (= 2 (aget mid 512))))
        (finally (src/s-close c))))))

(deftest vhd-footer-is-a-valid-fixed-footer
  (let [f (restore/vhd-footer (* 64 mib))]
    (is (= 512 (alength f)))
    (is (= "conectix" (String. f 0 8 "US-ASCII")))
    (let [zeroed (java.util.Arrays/copyOf f 512)
          _ (dotimes [i 4] (aset zeroed (+ 64 i) (byte 0)))
          sum (reduce (fn [^long s x] (+ s (bit-and (long x) 0xff))) 0 (seq zeroed))
          stored (.getInt (.order (java.nio.ByteBuffer/wrap f) java.nio.ByteOrder/BIG_ENDIAN) 64)]
      (is (= (unchecked-int (bit-not sum)) stored)))
    (is (= 2 (.getInt (.order (java.nio.ByteBuffer/wrap f) java.nio.ByteOrder/BIG_ENDIAN) 60)))
    (is (= (* 64 mib) (.getLong (.order (java.nio.ByteBuffer/wrap f) java.nio.ByteOrder/BIG_ENDIAN) 48)))))

(deftest compression-round-trips-and-never-grows
  (doseq [spec ["none" "lz4" "zstd" "zstd,9"]]
    (let [k (key/plaintext-key spec)
          zeros (byte-array mib)
          noise (byte-array mib)]
      (.nextBytes (java.util.Random. 5) noise)
      (doseq [^bytes d [zeros noise]]
        (let [e (key/encrypt k d)]
          (is (Arrays/equals d ^bytes (key/decrypt k nil e)) spec)
          (is (<= (alength e) (+ 3 (alength d))) spec)))
      (when (not= "none" spec)
        (is (< (alength (key/encrypt k zeros)) 100000) spec)))))

(deftest parse-target-tells-devices-from-files
  (is (= {:kind :disk :disk 1} (wv/parse-target "disk:1")))
  (is (= {:kind :disk :disk 0} (wv/parse-target "\\\\.\\PhysicalDrive0")))
  (is (= {:kind :volume :letter "D"} (wv/parse-target "D:")))
  (is (= {:kind :file :path "C:\\tmp\\x.img"} (wv/parse-target "C:\\tmp\\x.img")))
  (is (= {:kind :file :path "/tmp/x.img"} (wv/parse-target "/tmp/x.img"))))

(defn- with-oracle [f]
  (when-let [o (fx/oracle)]
    (let [dir (tmpdir)
          repo (str (.resolve dir "repo"))
          run (fn [& args]
                (let [pb (ProcessBuilder. ^java.util.List (vec (cons o args)))]
                  (.putAll (.environment pb) {"BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK" "yes"})
                  (.redirectErrorStream pb true)
                  (let [p (.start pb) out (slurp (.getInputStream p))]
                    (when-not (zero? (.waitFor p)) (throw (ex-info "oracle failed" {:args args :out out})))
                    out)))]
      (run "init" "--encryption=none" repo)
      (f {:dir dir :repo repo :oracle o :run run}))))

(defn- shoot! [{:keys [repo oracle]} archive source opts]
  (let [conn (remote/spawn [oracle "serve"])]
    (try
      (let [rid (b/hex (remote/open! conn repo))]
        (create/create-image! conn (key/plaintext-key (:compression opts "zstd,3"))
                              archive [source]
                              (merge {:repo-id rid :block-size (* 4 mib)} opts)))
      (finally (remote/close conn)))))

(deftest image-round-trips-through-a-real-borg
  (with-oracle
    (fn [{:keys [dir repo oracle run] :as ctx}]
      (let [f (.resolve dir "disk.raw")
            raw (byte-array (* 32 mib))
            _ (.nextBytes (java.util.Random. 11) raw)
            _ (write! f raw)
            used [[0 (* 4 mib)] [(* 12 mib) (* 8 mib)]]
            want (let [o (byte-array (* 32 mib))]
                   (doseq [[s l] used] (System/arraycopy raw (int s) o (int s) (int l)))
                   o)
            s (src/file-source f used)
            res (try (shoot! ctx "img1" s {}) (finally (src/s-close s)))]
        (testing "only allocated clusters are read"
          (is (= (* 12 mib) (:read res)))
          (is (= (* 20 mib) (:zeroed res)))
          (is (= (* 32 mib) (:size res))))
        (testing "one chunk per allocated block plus a single shared zero chunk"
          (is (= 8 (:blocks res)))
          (is (= 4 (:new-chunks res))))
        (testing "upstream borg reads the archive"
          (let [out (.resolve dir "out")]
            (Files/createDirectories out (into-array java.nio.file.attribute.FileAttribute []))
            (let [pb (ProcessBuilder. ^java.util.List [oracle "extract" (str repo "::img1")])]
              (.putAll (.environment pb) {"BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK" "yes"})
              (.directory pb (.toFile out))
              (.redirectErrorStream pb true)
              (let [p (.start pb)] (slurp (.getInputStream p)) (is (zero? (.waitFor p)))))
            (is (Arrays/equals want (Files/readAllBytes (.resolve out "disk.raw.img"))))))))))

(defn- serve+process
  "A connection to a real borg serve, plus its process -- so a run can be
   killed the way a run really dies, without the chance to commit or clean
   up."
  [oracle]
  (let [pb (doto (ProcessBuilder. ^java.util.List [oracle "serve"])
             (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))
        p (.start pb)]
    [(remote/->Conn (java.io.BufferedOutputStream. (.getOutputStream p))
                    (java.io.BufferedInputStream. (.getInputStream p))
                    (atom 0) (atom (byte-array 0)) (java.util.HashSet.)
                    (fn [] (.close (.getOutputStream p)) (.waitFor p)))
     p]))

(deftest an-aborted-run-does-not-block-the-next-one
  (with-oracle
    (fn [{:keys [dir oracle]}]
      (let [repo (str (.resolve dir "frisch"))
            k (key/plaintext-key "none")
            blast! (fn [conn n]
                     (let [r (java.util.Random. 5)]
                       (dotimes [_ n]
                         (let [d (byte-array mib)]
                           (.nextBytes r d)
                           (remote/put! conn (key/id-hash k d) (key/encrypt k d))))
                       (remote/drain-puts! conn)))]
        (let [[c1 p1] (serve+process oracle)]
          (remote/open! c1 repo :create true)
          (is (true? (create/init-repo! c1 k)) "das leere Manifest wird beim Anlegen geschrieben")
          (blast! c1 3)
          (.destroyForcibly p1)
          (.waitFor p1))
        (testing "the interrupted run left a committed transaction behind to roll back to"
          (is (.exists (java.io.File. (str repo "/index.1")))))
        (let [[c2 _] (serve+process oracle)]
          (remote/open! c2 repo :create true)
          (is (nil? (create/init-repo! c2 k)) "und ein zweites Mal gibt es nichts zu tun")
          (blast! c2 1)
          (remote/commit! c2)
          (testing "the next run writes instead of tripping over data/0/0"
            (is (seq (filter #(str/starts-with? % "index.")
                             (map str (vec (.list (java.io.File. repo))))))))
          (remote/close c2))))))

(deftest the-free-tail-is-hashed-once-and-then-only-looked-up
  (with-oracle
    (fn [{:keys [dir] :as ctx}]
      (let [f (.resolve dir "disk.raw")
            raw (byte-array (* 32 mib))
            _ (.nextBytes (java.util.Random. 23) raw)
            _ (write! f raw)
            used [[0 (* 4 mib)]]
            blocks (atom 0)
            orig key/id-hash
            s (src/file-source f used)
            res (with-redefs [key/id-hash (fn [k ^bytes d]
                                            (when (= (alength d) (* 4 mib)) (swap! blocks inc))
                                            (orig k d))]
                  (try (shoot! ctx "tail" s {}) (finally (src/s-close s))))]
        (testing "the one allocated block and the first free one are hashed, the other six are not"
          (is (= 2 @blocks))
          (is (= 8 (:blocks res))))
        (testing "and the zero chunk is stored exactly once"
          (is (= 2 (:new-chunks res)))
          (is (= (* 4 mib) (:read res)))
          (is (= (* 28 mib) (:zeroed res))))))))

(deftest every-compression-we-write-is-read-back-by-borg
  (with-oracle
    (fn [{:keys [dir repo oracle] :as ctx}]
      ;; the chunk id is taken over the plaintext, so the same bytes under a
      (doseq [spec ["none" "lz4" "zstd" "zstd,1" "zstd,22"]]
        (testing spec
          (let [tag (str/replace spec "," "-")
                f (.resolve dir (str "c-" tag ".raw"))
                raw (byte-array (* 8 mib))
                _ (.nextBytes (java.util.Random. (hash spec)) raw)
                ;; one block that no compressor can shrink, one that every
                _ (Arrays/fill raw (* 4 mib) (* 8 mib) (byte 0x41))
                _ (write! f raw)
                s (src/file-source f [[0 (* 8 mib)]])]
            (try (shoot! ctx tag s {:compression spec}) (finally (src/s-close s)))
            (let [out (.resolve dir (str "out-" tag))]
              (Files/createDirectories out (into-array java.nio.file.attribute.FileAttribute []))
              (let [pb (ProcessBuilder. ^java.util.List [oracle "extract" (str repo "::" tag)])]
                (.putAll (.environment pb) {"BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK" "yes"})
                (.directory pb (.toFile out))
                (.redirectErrorStream pb true)
                (let [p (.start pb)] (slurp (.getInputStream p)) (is (zero? (.waitFor p)) spec)))
              (is (Arrays/equals raw (Files/readAllBytes (.resolve out (str "c-" tag ".raw.img")))) spec))))))))

(deftest a-stopped-run-commits-nothing
  (with-oracle
    (fn [{:keys [dir repo oracle run]}]
      (let [f (.resolve dir "halb.raw")
            raw (byte-array (* 32 mib))
            _ (.nextBytes (java.util.Random. 41) raw)
            _ (write! f raw)
            seen (atom 0)
            s (src/file-source f [[0 (* 32 mib)]])
            conn (remote/spawn [oracle "serve"])
            e (try
                (let [rid (b/hex (remote/open! conn repo))]
                  (create/create-image! conn (key/plaintext-key "none") "halb" [s]
                                        {:repo-id rid :block-size (* 4 mib)
                                         :stop? #(> (swap! seen inc) 3)})
                  nil)
                (catch clojure.lang.ExceptionInfo ex ex)
                (finally (remote/close conn) (src/s-close s)))]
        (testing "the run stops where it was asked to, and says so"
          (is (some? e))
          (is (true? (:stopped (ex-data e))))
          (is (= 3 (:block (ex-data e))))
          (is (= 8 (:of (ex-data e)))))
        (testing "and no half image is left behind that would pass for a backup"
          ;; the stop is thrown rather than returned precisely so that it
          (is (not (str/includes? (run "list" repo) "halb"))))))))

(deftest a-changed-block-is-the-only-thing-sent
  (with-oracle
    (fn [{:keys [dir] :as ctx}]
      (let [f (.resolve dir "disk.raw")
            raw (byte-array (* 32 mib))
            _ (.nextBytes (java.util.Random. 13) raw)
            _ (write! f raw)
            used [[0 (* 32 mib)]]
            s1 (src/file-source f used)
            r1 (try (shoot! ctx "a1" s1 {}) (finally (src/s-close s1)))]
        (is (= 8 (:new-chunks r1)))
        (let [all (Files/readAllBytes f)]
          (dotimes [i 4096] (aset all (int (+ (* 5 mib) i)) (unchecked-byte (mod i 97))))
          (write! f all))
        (let [s2 (src/file-source f used)
              r2 (try (shoot! ctx "a2" s2 {}) (finally (src/s-close s2)))]
          (testing "the other seven blocks deduplicate against the repository"
            (is (= 1 (:new-chunks r2)))
            (is (= (* 8 (* 4 mib)) (:size r2)))
            (is (< (long (:sent r2)) (* 5 mib)))))))))

(deftest restore-writes-the-image-back-byte-for-byte
  (with-oracle
    (fn [{:keys [dir repo oracle] :as ctx}]
      (let [f (.resolve dir "disk.raw")
            raw (byte-array (* 16 mib))
            _ (.nextBytes (java.util.Random. 17) raw)
            _ (write! f raw)
            used [[0 (* 4 mib)] [(* 8 mib) (* 4 mib)]]
            want (let [o (byte-array (* 16 mib))]
                   (doseq [[s l] used] (System/arraycopy raw (int s) o (int s) (int l)))
                   o)
            s (src/file-source f used)
            _ (try (shoot! ctx "r1" s {}) (finally (src/s-close s)))
            conn (remote/spawn [oracle "serve"])
            st (store/remote conn repo)
            k (key/plaintext-key)]
        (try
          (let [m (arch/manifest st k)
                items (arch/items st k (arch/archive st k m "r1"))
                item (restore/pick-image items nil)
                info (restore/manifest-of st k items)
                dst (.resolve dir "restored.img")
                sink (restore/file-sink dst)]
            (is (= (* 4 mib) (:block-size info)))
            (is (= :zeroed (:free-space info)))
            (is (= "disk.raw.img" (:image (first (:sources info)))))
            (let [res (try (restore/write-image! st k item sink {:skip-zeros false})
                           (finally (restore/sink-close sink)))]
              (is (= (* 16 mib) (:length res)))
              (is (Arrays/equals want (Files/readAllBytes dst))))
            (testing "skip-zeros leaves a sparse file of the same content"
              (let [d2 (.resolve dir "sparse.img")
                    sink2 (restore/file-sink d2)
                    res2 (try (restore/write-image! st k item sink2 {:skip-zeros true})
                              (finally (restore/sink-close sink2)))]
                (is (= (* 8 mib) (:written res2)))
                (is (= (* 8 mib) (:skipped res2)))))
            (testing "a skipped trailing zero block still leaves a full-length file"
              (let [d3 (.resolve dir "tail.img")
                    sink3 (restore/file-sink d3)]
                (try
                  (let [r3 (restore/write-image! st k item sink3 {:skip-zeros true})]
                    (restore/set-size! sink3 (:length r3)))
                  (finally (restore/sink-close sink3)))
                (is (= (* 16 mib) (Files/size d3)))
                (is (Arrays/equals want (Files/readAllBytes d3))))))
          (finally (store/close st)))))))

(def ^:private gpt-disk
  {:kind :disk :disk 0 :sector-size 512 :length (* 2048 mib) :style :gpt
   :disk-id "1d9a2b3c-4e5f-4a6b-8c7d-9e0f1a2b3c4d"
   :partitions [{:number 1 :offset 1048576 :length 104857600
                 :type-guid "c12a7328-f81f-11d2-ba4b-00a0c93ec93b"
                 :part-guid "aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa" :label "EFI system partition"}
                {:number 2 :offset 105906176 :length 16777216
                 :type-guid "e3c9e316-0b5c-4db8-817d-f92df00215ae"
                 :part-guid "bbbbbbbb-2222-4222-8222-bbbbbbbbbbbb" :label "Microsoft reserved"}
                {:number 3 :offset 122683392 :length 2024751104
                 :type-guid "ebd0a0a2-b9e5-4433-87c0-68b6b72699c7"
                 :part-guid "cccccccc-3333-4333-8333-cccccccccccc" :label "Basic data partition"}]
   :volumes [{:offset 122683392 :length 2024751104 :letter "C"
              :allocated-bytes (* 600 mib)
              :highest-allocated-byte (* 700 mib)}]})

(deftest layout-keeps-every-guid-so-the-bcd-still-resolves
  (let [out (cli/layout-script gpt-disk (* 2048 mib) false)]
    (is (str/includes? out "label: gpt"))
    (is (str/includes? out "label-id: 1d9a2b3c-4e5f-4a6b-8c7d-9e0f1a2b3c4d"))
    (doseq [g ["aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
               "bbbbbbbb-2222-4222-8222-bbbbbbbbbbbb"
               "cccccccc-3333-4333-8333-cccccccccccc"]]
      (is (str/includes? out (str "uuid=" g)) g))
    (is (str/includes? out "start=2048, size=204800"))
    (is (str/includes? out "name=\"EFI system partition\""))))

(deftest shrinking-moves-only-the-largest-partition
  (let [parts (#'cli/shrink-plan gpt-disk (* 1536 mib) 512 false)
        by-n (into {} (map (juxt :number identity)) parts)]
    (testing "the partitions before it keep their exact offsets"
      (is (= 1048576 (:offset (by-n 1))))
      (is (= 105906176 (:offset (by-n 2))))
      (is (= 104857600 (:length (by-n 1)))))
    (testing "the largest one absorbs the whole deficit"
      (is (= 122683392 (:offset (by-n 3))))
      (is (= (- 2024751104 (* 512 mib)) (:length (by-n 3)))))
    (testing "everything still fits"
      (is (<= (+ (long (:offset (by-n 3))) (long (:length (by-n 3)))) (* 1536 mib))))))

(deftest shrinking-unter-die-belegten-bytes-wird-abgelehnt
  ;; Die Grenze sind die belegten Bytes plus Luft, nicht der hoechste belegte
  (is (thrown-with-msg? Exception #"needs at least"
                        (cli/layout-script gpt-disk (* 768 mib) false)))
  (is (string? (cli/layout-script gpt-disk (* 768 mib) true)))
  (testing "ueber dem hoechsten belegten Cluster, aber ueber der Grenze: geht"
    (is (string? (cli/layout-script gpt-disk (* 900 mib) false)))))

(deftest die-schrumpfgrenze-liegt-nie-ueber-dem-datentraeger
  ;; Ein fast volles Abbild darf keine Grenze bekommen, die groesser ist als der
  (let [voll {:allocated-bytes (* 190 mib)}]
    (is (= (* 192 mib) (cli/shrink-floor voll (* 192 mib))))))

(deftest mbr-layout-emits-a-hex-signature-and-a-boot-flag
  (let [mbr {:kind :disk :disk 0 :sector-size 512 :length (* 512 mib) :style :mbr
             :mbr-signature 0x1234abcd
             :partitions [{:number 1 :offset 1048576 :length (* 500 mib)
                           :style :mbr :mbr-type 7 :boot true}]
             :volumes []}
        out (cli/layout-script mbr (* 512 mib) false)]
    (is (str/includes? out "label: dos"))
    (is (str/includes? out "label-id: 0x1234abcd"))
    (is (str/includes? out "type=07"))
    (is (str/includes? out "bootable"))))

(deftest a-partition-range-restores-at-offset-zero
  (with-oracle
    (fn [{:keys [dir] :as ctx}]
      (let [f (.resolve dir "disk.raw")
            raw (byte-array (* 16 mib))
            _ (.nextBytes (java.util.Random. 23) raw)
            _ (write! f raw)
            s (src/file-source f [[0 (* 16 mib)]])
            _ (try (shoot! ctx "d1" s {}) (finally (src/s-close s)))
            {:keys [repo oracle]} ctx
            conn (remote/spawn [oracle "serve"])
            st (store/remote conn repo)
            k (key/plaintext-key)]
        (try
          (let [m (arch/manifest st k)
                items (arch/items st k (arch/archive st k m "d1"))
                item (restore/pick-image items nil)
                dst (.resolve dir "part.img")
                sink (restore/file-sink dst)
                from (* 5 mib) span (* 3 mib)]
            (let [res (try (restore/write-image! st k item sink {:from from :span span})
                           (finally (restore/sink-close sink)))]
              (is (= span (:length res)))
              (is (= span (:written res))))
            (is (= span (Files/size dst)))
            (is (Arrays/equals (Arrays/copyOfRange raw (int from) (int (+ from span)))
                               (Files/readAllBytes dst))))
          (finally (store/close st)))))))

(deftest cuts-restart-the-block-grid
  (testing "no cuts is the plain fixed grid"
    (is (= [[0 100] [100 100] [200 50]] (image/blocks-for 250 100 []))))
  (testing "a cut restarts it, so the run is measured from its own start"
    (is (= [[0 30] [30 100] [130 100] [230 20]] (image/blocks-for 250 100 [30])))
    (is (= [[0 30] [30 100] [130 40] [170 80]] (image/blocks-for 250 100 [30 170]))))
  (testing "cuts outside the volume and duplicates are ignored"
    (is (= [[0 100] [100 100] [200 50]] (image/blocks-for 250 100 [0 250 -5 900])))))

(deftest prune-cuts-bounds-the-chunk-count
  (is (= [1000 5000] (src/prune-cuts [1000 1010 1020 5000] 512 10000)))
  (is (= [] (src/prune-cuts [0 10000] 512 10000)))
  (is (= [4096] (src/prune-cuts [4096 4096 4200] 512 10000))))

(deftest subtract-removes-excluded-extents
  (is (= [[0 512] [1536 512]] (src/subtract [[0 2048]] [[512 1024]] 2048 512)))
  (is (= [] (src/subtract [[0 1024]] [[0 1024]] 1024 512)))
  (is (= [[0 1024]] (src/subtract [[0 1024]] [] 1024 512)))
  (is (= [[0 512]] (src/subtract [[0 1024]] [[512 512]] 1024 512))))

(defn- lay-out
  "Writes payloads at cluster-aligned offsets after a machine-unique header."
  [^long size ^long header payloads]
  (let [vol (byte-array size)
        rnd (java.util.Random. (long (hash (vec (map alength payloads)))))]
    (.nextBytes (java.util.Random. (System/nanoTime)) (byte-array 0))
    (let [hdr (byte-array header)]
      (.nextBytes rnd hdr)
      (System/arraycopy hdr 0 vol 0 header))
    (loop [pos header [p & more] payloads cuts []]
      (if-not p
        [vol cuts]
        (let [len (alength ^bytes p)
              end (+ pos (* 4096 (quot (+ len 4095) 4096)))]
          (System/arraycopy p 0 vol (int pos) len)
          (recur end more (conj cuts pos end)))))))

(deftest run-relative-chunking-deduplicates-across-machines
  (with-oracle
    (fn [{:keys [dir] :as ctx}]
      (let [payloads (mapv (fn [n] (let [b (byte-array n)]
                                     (.nextBytes (java.util.Random. (+ 100 n)) b) b))
                           [(* 3 mib) (* 5 mib) (* 2 mib) (* 6 mib) (* 4 mib)])
            size (* 48 mib)
            [volA cutsA] (lay-out size (* 1 mib) payloads)
            [volB cutsB] (lay-out size (* 3 mib) (reverse payloads))
            fa (.resolve dir "a.vol") fb (.resolve dir "b.vol")
            _ (do (write! fa volA) (write! fb volB))
            shoot (fn [arch f cuts]
                    (let [s (src/file-source f [[0 size]] cuts)]
                      (try (shoot! ctx arch s {}) (finally (src/s-close s)))))]
        (testing "fixed grid: the second machine shares almost nothing"
          (let [_ (shoot "fixA" fa nil)
                b (shoot "fixB" fb nil)]
            (is (> (long (:sent b)) (* 15 mib))
                "with a fixed grid the identical payloads land on different boundaries")))
        (testing "run-relative: the identical payloads deduplicate"
          (let [_ (shoot "relA" fa cutsA)
                b (shoot "relB" fb cutsB)]
            (is (< (long (:sent b)) (* 8 mib))
                "the payload blocks are byte-identical once the grid restarts at each run")))))))

(deftest large-item-stream-spans-chunk-boundaries
  ;; Regression: an item whose chunk list is large makes its packed msgpack map
  (let [big-item {:path "C.img" :mode 0100644
                  :chunks (vec (for [i (range 20000)]
                                 [(byte-array (repeat 32 (unchecked-byte i))) 4194304 4194304]))}
        small {:path "imager.edn" :mode 0100644 :chunks []}
        stream (let [bos (java.io.ByteArrayOutputStream.)]
                 (.write bos ^bytes (borgclj.mpack/pack big-item))
                 (.write bos ^bytes (borgclj.mpack/pack small))
                 (.toByteArray bos))
        chunks (borgclj.chunker/chunk-stream stream 0 borgclj.chunker/items-params)]
    (testing "the stream really did split into multiple item chunks"
      (is (> (count chunks) 1)))
    (testing "per-chunk unpack fails on the straddling item (the old bug)"
      (is (thrown? Exception
            (mapcat #(borgclj.msgpack/unpack-stream %) chunks))))
    (testing "concatenate-then-unpack recovers both items intact"
      (let [cat (let [bos (java.io.ByteArrayOutputStream.)]
                  (doseq [c chunks] (.write bos ^bytes c))
                  (.toByteArray bos))
            items (borgclj.msgpack/unpack-stream cat)]
        (is (= 2 (count items)))
        (is (= "C.img" (:path (first items))))
        (is (= 20000 (count (:chunks (first items)))))
        (is (= "imager.edn" (:path (second items))))))))
