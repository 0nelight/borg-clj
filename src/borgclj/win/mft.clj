(ns borgclj.win.mft
  (:require [borgclj.win.ffm :as ffm]
            [borgclj.win.ioctl :as io]
            [clojure.string :as str])
  (:import [java.lang.invoke MethodHandle]
           [java.lang.foreign Arena FunctionDescriptor Linker Linker$Option
            MemoryLayout MemorySegment SymbolLookup ValueLayout]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.charset StandardCharsets]))

(def ^:const root-frn 5)
(def ^:const attr-directory 0x10)
(def ^:const attr-reparse 0x400)

(def default-excludes
  ["pagefile.sys"
   "hiberfil.sys"
   "swapfile.sys"
   "System Volume Information"
   "$Recycle.Bin"])

(def ^:private linker (delay (Linker/nativeLinker)))
(def ^:private k32 (delay (SymbolLookup/libraryLookup "kernel32.dll" (Arena/global))))

(def ^:private h-open-by-id
  (delay
    (.downcallHandle ^Linker @linker
                     (.orElseThrow (.find ^SymbolLookup @k32 "OpenFileById"))
                     (FunctionDescriptor/of ValueLayout/ADDRESS
                                            (into-array MemoryLayout
                                                        [ValueLayout/ADDRESS ValueLayout/ADDRESS
                                                         ValueLayout/JAVA_INT ValueLayout/JAVA_INT
                                                         ValueLayout/ADDRESS ValueLayout/JAVA_INT]))
                     (into-array Linker$Option
                                 [(Linker$Option/captureCallState (into-array String ["GetLastError"]))]))))

(defn- le ^ByteBuffer [^bytes b] (.order (ByteBuffer/wrap b) ByteOrder/LITTLE_ENDIAN))

(defn- enum-input ^bytes [^long start-frn ^long high-usn]
  (let [b (.order (ByteBuffer/allocate 24) ByteOrder/LITTLE_ENDIAN)]
    (.putLong b start-frn)
    (.putLong b 0)
    (.putLong b high-usn)
    (.array b)))

(defn throttled
  "Wraps a reporting function so it is called at most once a second."
  [f]
  (if-not f
    (fn [_])
    (let [last (volatile! 0)]
      (fn [m]
        (let [now (System/nanoTime)]
          (when (>= (- now (long @last)) 1000000000)
            (vreset! last now)
            (f m)))))))

(defn records
  "Every file record on the volume as {:frn :parent :name :attrs}."
  ([dev] (records dev (throttled nil)))
  ([dev report]
  (let [high (or (:next-usn (io/usn-journal dev)) Long/MAX_VALUE)
        cap (* 1024 1024)]
    (loop [next-frn 0 acc (transient [])]
      (let [{:keys [error out bytes]}
            (ffm/ioctl dev io/fsctl-enum-usn-data (enum-input next-frn high) (+ 8 cap)
                       :tolerate #{ffm/error-handle-eof 38 1 50 87})]
        (if (or error (nil? bytes) (<= (long bytes) 8))
          (persistent! acc)
          (let [b (le out)
                nxt (.getLong b 0)
                stop (long bytes)]
            (let [acc (loop [o 8 acc acc]
                        (if (>= o stop)
                          acc
                          (let [rec-len (bit-and (long (.getInt b o)) 0xffffffff)]
                            (if (or (< rec-len 60) (> (+ o rec-len) stop))
                              acc
                              (let [name-len (bit-and (.getShort b (+ o 56)) 0xffff)
                                    name-off (bit-and (.getShort b (+ o 58)) 0xffff)
                                    nm (String. out (int (+ o name-off)) (int name-len)
                                                StandardCharsets/UTF_16LE)]
                                (recur (+ o rec-len)
                                       (conj! acc {:frn (.getLong b (+ o 8))
                                                   :parent (.getLong b (+ o 16))
                                                   :attrs (bit-and (long (.getInt b (+ o 52))) 0xffffffff)
                                                   :name nm})))))))]
              (if (= nxt next-frn)
                (persistent! acc)
                (do (report {:phase :mft-records :records (count acc)})
                    (recur nxt acc)))))))))))

(def ^:const file-id-both-dir-info 10)
(def ^:const file-id-both-dir-restart 11)
(def ^:const list-directory 1)

(defn- open-by-id
  ([dev ^long frn] (open-by-id dev frn 0x80))
  ([dev ^long frn ^long access]
  (with-open [a (Arena/ofConfined)]
    (let [cap (.allocate a ^MemoryLayout (Linker$Option/captureStateLayout))
          desc (.allocate a 24 8)]
      (.set desc ValueLayout/JAVA_INT 0 (unchecked-int 24))
      (.set desc ValueLayout/JAVA_INT 4 (unchecked-int 0))
      (.set desc ValueLayout/JAVA_LONG 8 frn)
      (let [seg (.invokeWithArguments ^MethodHandle @h-open-by-id
                                      ^java.util.List [cap (:h dev) desc
                                                       (unchecked-int access)
                                                       (unchecked-int 7)
                                                       MemorySegment/NULL
                                                       (unchecked-int ffm/flag-backup-semantics)])
            addr (.address ^MemorySegment seg)]
        (when-not (or (= addr -1) (= addr 0xffffffffffffffff) (zero? addr))
          seg))))))

(defn retrieval-runs
  "Cluster runs [[lcn count] ...] of one file, or nil when it has none
   (resident, sparse, or not openable)."
  [dev ^long frn]
  (when-let [h (open-by-id dev frn)]
    (let [fake (assoc dev :h h)]
      (try
        (loop [vcn 0 acc []]
          (let [in (.array (.putLong (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN) vcn))
                {:keys [error out bytes]}
                (ffm/ioctl fake io/fsctl-get-retrieval-ptrs in (+ 32 (* 512 16))
                           :tolerate #{ffm/error-more-data 1 50 87 5 38})]
            (if (and error (not= ffm/error-more-data error))
              (seq acc)
              (let [b (le out)
                    n (bit-and (long (.getInt b 0)) 0xffffffff)
                    start (.getLong b 8)
                    n (min n (quot (- (long (or bytes 0)) 16) 16))]
                (if-not (pos? n)
                  (seq acc)
                  (let [[acc last-vcn]
                        (loop [i 0 prev start acc acc]
                          (if (>= i n)
                            [acc prev]
                            (let [o (+ 16 (* i 16))
                                  nv (.getLong b o)
                                  lcn (.getLong b (+ o 8))]
                              (recur (inc i) nv
                                     (if (neg? lcn) acc (conj acc [lcn (- nv prev)]))))))]
                    (if (= ffm/error-more-data error)
                      (recur last-vcn acc)
                      (seq acc))))))))
        (finally
          (ffm/close-handle h))))))

(defn dir-entries
  "Every entry of one directory as {:frn :size :attrs}, read in bulk."
  [dev ^long dir-frn]
  (when-let [h (open-by-id dev dir-frn list-directory)]
    (try
      (loop [klass file-id-both-dir-restart acc (transient [])]
        (if-let [^bytes buf (ffm/file-info-by-handle h klass 65536)]
          (let [b (le buf)
                acc (loop [o 0 acc acc]
                      (let [nxt (bit-and (long (.getInt b o)) 0xffffffff)
                            attrs (bit-and (long (.getInt b (+ o 56))) 0xffffffff)
                            e (conj! acc {:frn (.getLong b (+ o 96))
                                          :size (.getLong b (+ o 40))
                                          :attrs attrs})]
                        (if (zero? nxt) e (recur (+ o nxt) e))))]
            (recur file-id-both-dir-info acc))
          (persistent! acc)))
      (finally (ffm/close-handle h)))))

(defn- path-of [by-frn frn]
  (loop [f (long frn) parts () guard 0]
    (let [r (get by-frn f)]
      (cond
        (or (nil? r) (> guard 64)) (str/join "\\" parts)
        (= (long (:frn r)) root-frn) (str/join "\\" parts)
        :else (recur (long (:parent r)) (conj parts (:name r)) (inc guard))))))

(defn- excluded? [patterns ^String path]
  (let [lc (str/lower-case path)]
    (boolean (some (fn [^String p]
                     (let [pl (str/lower-case p)]
                       (or (= lc pl)
                           (str/starts-with? lc (str pl "\\")))))
                   patterns))))

(defn scan
  "File-run cuts and excluded byte extents for one volume."
  [dev {:keys [cluster-size min-run excludes progress]
        :or {min-run (* 256 1024) excludes default-excludes}}]
  (let [cluster (long cluster-size)
        min-run (long min-run)
        report (throttled progress)
        recs (records dev report)
        by-frn (persistent!
                 (reduce (fn [m r] (assoc! m (:frn r) r)) (transient {}) recs))
        dirs (filter #(pos? (bit-and (long (:attrs %)) attr-directory)) recs)
        drop-frns (when (seq excludes)
                    (into #{} (comp (remove #(pos? (bit-and (long (:attrs %)) attr-directory)))
                                    (filter #(excluded? excludes (path-of by-frn (:frn %))))
                                    (map :frn))
                          recs))
        dirs-total (count dirs)]
    (loop [[d & more] dirs
           cuts (transient []) excl (transient [])
           dirs-seen 0 opened 0 runs 0]
      (if-not d
        {:cuts (persistent! cuts) :excluded (persistent! excl)
         :files opened :runs runs :records (count recs)
         :dirs dirs-seen :excluded-files (count drop-frns)}
        (let [entries (try (dir-entries dev (:frn d)) (catch Throwable _ nil))
              want (filter (fn [e]
                             (and (not (pos? (bit-and (long (:attrs e)) attr-directory)))
                                  (or (contains? drop-frns (:frn e))
                                      (>= (long (:size e)) min-run))))
                           entries)]
          (let [[cuts excl opened runs]
                (reduce (fn [[c x o n] e]
                          (let [drop? (contains? drop-frns (:frn e))
                                rs (try (retrieval-runs dev (:frn e)) (catch Throwable _ nil))]
                            (if-not rs
                              [c x (inc (long o)) n]
                              (let [[c x n]
                                    (reduce (fn [[c x n] [lcn cnt]]
                                              (let [off (* (long lcn) cluster)
                                                    len (* (long cnt) cluster)]
                                                (cond
                                                  drop? [c (conj! x [off len]) (inc (long n))]
                                                  (>= len min-run) [(conj! (conj! c off) (+ off len)) x (inc (long n))]
                                                  :else [c x (inc (long n))])))
                                            [c x n] rs)]
                                [c x (inc (long o)) n]))))
                        [cuts excl opened runs] want)]
            (report {:phase :mft-dirs :dirs (inc (long dirs-seen)) :dirs-total dirs-total
                     :files opened :runs runs
                     :percent (if (pos? dirs-total)
                                (* 100.0 (/ (double (inc (long dirs-seen))) (double dirs-total)))
                                0.0)})
            (recur more cuts excl (inc (long dirs-seen)) opened runs)))))))
