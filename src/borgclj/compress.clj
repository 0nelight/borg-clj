(ns borgclj.compress
  (:require [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.util.zip Inflater Deflater]
           [net.jpountz.lz4 LZ4Factory]
           [com.github.luben.zstd Zstd ZstdInputStream]
           [org.tukaani.xz XZInputStream]))

(def ids {:none [0x00 0x00] :lz4 [0x01 0x00] :lzma [0x02 0x00]
          :zstd [0x03 0x00] :obfuscate [0x04 0x00] :zlib [0x08 0x00]})

(defn- drain ^bytes [in]
  (let [out (ByteArrayOutputStream.)
        buf (byte-array 65536)]
    (loop []
      (let [n (.read ^java.io.InputStream in buf)]
        (when (pos? n) (.write out buf 0 n) (recur))))
    (.toByteArray out)))

(defn- tail ^bytes [^bytes b ^long off]
  (java.util.Arrays/copyOfRange b (int off) (alength b)))

(defn- zlib? [^bytes b]
  (let [b0 (bit-and (aget b 0) 0xff) b1 (bit-and (aget b 1) 0xff)]
    (and (= 8 (bit-and b0 0x0f)) (zero? (mod (+ (* b0 256) b1) 31)))))

(defn- inflate ^bytes [^bytes b]
  (let [inf (Inflater.) out (ByteArrayOutputStream.) buf (byte-array 65536)]
    (.setInput inf b)
    (loop []
      (when-not (.finished inf)
        (let [n (.inflate inf buf)]
          (when (pos? n) (.write out buf 0 n))
          (recur))))
    (.end inf)
    (.toByteArray out)))

(defn- lz4 ^bytes [^bytes b]
  (let [dec (.safeDecompressor (LZ4Factory/safeInstance))]
    (loop [cap (max 1024 (* 3 (alength b)))]
      (if (> cap (* 256 1024 1024))
        (throw (ex-info "lz4 output too large" {}))
        (let [dest (byte-array cap)]
          (if-let [n (try (.decompress dec b 0 (alength b) dest 0 cap)
                          (catch Exception _ nil))]
            (java.util.Arrays/copyOfRange dest 0 (int n))
            (recur (* 2 cap))))))))

(defn- unzstd ^bytes [^bytes b]
  (let [n (Zstd/decompressedSize b)]
    (if (pos? n)
      (Zstd/decompress b (int n))
      (drain (ZstdInputStream. (ByteArrayInputStream. b))))))

(defn decompress ^bytes [^bytes b]
  (let [b0 (bit-and (aget b 0) 0xff) b1 (bit-and (aget b 1) 0xff)]
    (cond
      (and (= b0 0x00) (= b1 0x00)) (tail b 2)
      (and (= b0 0x01) (= b1 0x00)) (lz4 (tail b 2))
      (and (= b0 0x02) (= b1 0x00)) (drain (XZInputStream. (ByteArrayInputStream. b 2 (- (alength b) 2))))
      (and (= b0 0x03) (= b1 0x00)) (unzstd (tail b 2))
      (and (= b0 0x04) (= b1 0x00)) (decompress (tail b 6))
      (zlib? b) (inflate b)
      :else (throw (ex-info "unknown compressor" {:id [b0 b1]})))))

(defn compress-none ^bytes [^bytes b]
  (let [out (byte-array (+ 2 (alength b)))]
    (System/arraycopy b 0 out 2 (alength b))
    out))

(defn- envelope ^bytes [^long id0 ^long id1 ^bytes payload]
  (let [out (byte-array (+ 2 (alength payload)))]
    (aset out 0 (unchecked-byte id0))
    (aset out 1 (unchecked-byte id1))
    (System/arraycopy payload 0 out 2 (alength payload))
    out))

(defn- lz4-block ^bytes [^bytes b]
  (let [c (.fastCompressor (LZ4Factory/fastestInstance))
        max- (.maxCompressedLength c (alength b))
        dst (byte-array max-)
        n (.compress c b 0 (alength b) dst 0 max-)]
    (java.util.Arrays/copyOfRange dst 0 n)))

(defn parse-spec
  "none | lz4 | zstd | zstd,LEVEL -- borg's own spelling with borg's own bounds
   (compress.pyx, CompressionSpec)."
  [s]
  (if (or (nil? s) (= "" s))
    {:kind :none}
    (let [parts (str/split (str/trim (str s)) #"," -1)
          named (first parts)
          kind (keyword (str/lower-case named))
          lvl (second parts)
          bad (fn [msg] (throw (ex-info msg {:spec s})))]
      (when (> (count parts) 2) (bad (str "too many arguments in compression spec: " s)))
      (case kind
        (:none :lz4) (do (when lvl (bad (str named " takes no level")))
                         {:kind kind})
        :zstd (let [n (long (if lvl
                              (or (parse-long lvl) (bad (str "not a compression level: " lvl)))
                              3))]
                (when-not (<= 1 n 22) (bad (str "zstd level must be between 1 and 22, got " n)))
                {:kind :zstd :level n})
        ;; decompress reads these, so archives written elsewhere restore fine --
        (:zlib :lzma) (bad (str named " can be read but not written -- use none, lz4 or zstd"))
        (bad (str "unknown compression: " named " -- use none, lz4 or zstd"))))))

(defn compress
  "Wraps data in a borg compressor envelope. Falls back to :none when the
   compressor did not actually shrink the chunk."
  ^bytes [{:keys [kind level] :or {kind :none level 3}} ^bytes b]
  (let [smaller (fn [^bytes c ^long i0 ^long i1]
                  (if (< (alength c) (alength b)) (envelope i0 i1 c) (envelope 0x00 0x00 b)))]
    (case kind
      :none (envelope 0x00 0x00 b)
      :lz4 (smaller (lz4-block b) 0x01 0x00)
      :zstd (smaller (Zstd/compress b (int (or level 3))) 0x03 0x00))))
