(ns borgclj.repository
  (:require [borgclj.bytes :as b]
            [clojure.string :as str])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel]
           [java.nio.file Files Path StandardOpenOption]
           [java.util.zip CRC32]))

(def magic "BORG_SEG")
(def ^:const tag-put 0)
(def ^:const tag-delete 1)
(def ^:const tag-commit 2)
(def ^:const header-size 9)
(def ^:const put-header-size 41)

(defn read-config [^Path repo]
  (let [txt (slurp (.toFile (.resolve repo "config")))]
    (into {} (for [line (str/split-lines txt)
                   :let [[k v] (map str/trim (str/split line #"=" 2))]
                   :when (and v (not (str/starts-with? k "[")))]
               [(keyword k) v]))))

(defn segments [^Path repo]
  (let [data (.resolve repo "data")]
    (->> (iterator-seq (.iterator (Files/newDirectoryStream data)))
         (mapcat (fn [d] (iterator-seq (.iterator (Files/newDirectoryStream ^Path d)))))
         (keep (fn [^Path f]
                 (let [n (str (.getFileName f))]
                   (when (re-matches #"\d+" n) [(parse-long n) f]))))
         (sort-by first))))

(defn- open-read ^FileChannel [^Path f]
  (FileChannel/open f (into-array StandardOpenOption [StandardOpenOption/READ])))

(defn- read-at ^ByteBuffer [^FileChannel ch ^long off ^long len]
  (let [bb (ByteBuffer/allocate (int len))]
    (loop [pos off]
      (when (pos? (.remaining bb))
        (let [n (.read ch bb pos)]
          (when (neg? n) (throw (ex-info "short read in segment" {:offset off})))
          (recur (+ pos n)))))
    (doto bb (.flip) (.order ByteOrder/LITTLE_ENDIAN))))

(defn entries
  "Object headers of one segment, without reading payloads."
  [^Path f]
  (with-open [ch (open-read f)]
    (let [size (.size ch)]
      (when-not (= magic (b/latin1 (.array ^ByteBuffer (read-at ch 0 8))))
        (throw (ex-info "not a borg segment" {:file (str f)})))
      (loop [off 8 acc []]
        (if (> (+ off header-size) size)
          acc
          (let [^ByteBuffer hb (read-at ch off header-size)
                esize (.getInt hb 4)
                tag (bit-and (.get hb 8) 0xff)]
            (if (= tag tag-commit)
              (recur (+ off esize) acc)
              (let [id (.array ^ByteBuffer (read-at ch (+ off header-size) 32))]
                (recur (+ off esize)
                       (conj acc {:tag tag :id id :offset off :size esize :file f}))))))))))

(defn index
  "id-hex -> {:file :offset :size}; later PUT wins, DELETE removes."
  [^Path repo]
  (reduce (fn [idx [_ f]]
            (reduce (fn [m {:keys [tag id offset size file]}]
                      (if (= tag tag-put)
                        (assoc m (b/hex id) {:file file :offset offset :size size})
                        (dissoc m (b/hex id))))
                    idx (entries f)))
          {} (segments repo)))

(defn- entry-size ^long [^Path file ^long offset]
  (with-open [ch (open-read file)]
    (.getInt ^ByteBuffer (read-at ch offset header-size) 4)))

(defn read-object ^bytes [{:keys [^Path file offset size]}]
  (let [size (long (or size (entry-size file offset)))]
    (with-open [ch (open-read file)]
    (let [^ByteBuffer bb (read-at ch offset size)
          ^bytes all (.array bb)
          crc (bit-and (.getInt bb 0) 0xffffffff)
          c (CRC32.)]
      (.update c all 4 (- (int size) 4))
      (when-not (= crc (.getValue c))
        (throw (ex-info "segment crc mismatch" {:file (str file) :offset offset})))
      (java.util.Arrays/copyOfRange all put-header-size (int size))))))

(def ^:private hashindex-magic "BORG_IDX")

(defn- newest-index [^Path repo]
  (->> (iterator-seq (.iterator (Files/newDirectoryStream repo "index.*")))
       (keep (fn [^Path f]
               (when-let [n (parse-long (subs (str (.getFileName f)) 6))] [n f])))
       (sort-by first)
       last
       second))

(defn read-hashindex
  "borg keeps its own id -> (segment, offset) table next to the segments."
  [^Path f]
  (let [^bytes b (Files/readAllBytes f)
        ^ByteBuffer bb (doto (ByteBuffer/wrap b) (.order ByteOrder/LITTLE_ENDIAN))]
    (when-not (= hashindex-magic (b/latin1 (java.util.Arrays/copyOfRange b 0 8)))
      (throw (ex-info "not a borg hash index" {:file (str f)})))
    (let [buckets (.getInt bb 12)
          ksize (bit-and (.get bb 16) 0xff)
          vsize (bit-and (.get bb 17) 0xff)
          bsize (+ ksize vsize)
          base 18]
      (when-not (= 32 ksize)
        (throw (ex-info "unexpected key size in hash index" {:key-size ksize})))
      (loop [i 0 acc (transient {})]
        (if (= i buckets)
          (persistent! acc)
          (let [off (+ base (* i bsize))
                seg (bit-and (long (.getInt bb (+ off ksize))) 0xffffffff)]
            (recur (inc i)
                   ;; 0xffffffff is an empty bucket, 0xfffffffe a deleted one
                   (if (>= seg 0xfffffffe)
                     acc
                     (assoc! acc (b/hex (java.util.Arrays/copyOfRange b off (+ off ksize)))
                             {:segment seg
                              :offset (bit-and (long (.getInt bb (+ off ksize 4))) 0xffffffff)
                              :size (when (>= vsize 12)
                                      (bit-and (long (.getInt bb (+ off ksize 8))) 0xffffffff))})))))))))

(defn index-from-hashindex
  "The borg index addresses objects by segment number; resolve those to files."
  [^Path repo]
  (when-let [f (newest-index repo)]
    (let [files (into {} (segments repo))]
      (persistent!
        (reduce-kv (fn [m hx {:keys [segment offset size]}]
                     (if-let [file (get files segment)]
                       (assoc! m hx {:file file :offset offset :size size})
                       m))
                   (transient {}) (read-hashindex f))))))

(defn open [^String p]
  (let [path (.toAbsolutePath (Path/of p (into-array String [])))]
    {:path path :config (read-config path)
     :idx (or (try (index-from-hashindex path)
                   (catch Exception _ nil))
              (index path))}))
