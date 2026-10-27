(ns borgclj.restore
  (:require [borgclj.archive :as arch]
            [borgclj.bytes :as b]
            [borgclj.image :as image]
            [borgclj.ntfs :as ntfs]
            [borgclj.parttable :as pt]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel]
           [java.nio.file Path StandardOpenOption]))

(defprotocol Sink
  (w! [_ off buf len])
  (sink-size [_])
  (set-size! [_ n])
  (sink-close [_]))

(defrecord FileSink [^FileChannel ch ^String name regular?]
  Sink
  (w! [_ off buf len]
    (let [bb (ByteBuffer/wrap buf 0 (int len))]
      (loop [pos (long off)]
        (when (pos? (.remaining bb))
          (recur (+ pos (.write ch bb pos)))))))
  (sink-size [_] (.size ch))
  (set-size! [_ n]
    (when regular?
      (let [n (long n)]
        (cond
          (> (.size ch) n) (.truncate ch n)
          (< (.size ch) n) (.write ch (ByteBuffer/allocate 1) (dec n))))))
  (sink-close [_] (.close ch)))

(defn file-sink [^Path p]
  (let [exists (java.nio.file.Files/exists p (into-array java.nio.file.LinkOption []))
        regular (or (not exists)
                    (java.nio.file.Files/isRegularFile p (into-array java.nio.file.LinkOption [])))]
    (->FileSink (FileChannel/open p (into-array StandardOpenOption
                                                [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                (str p) regular)))

(defn- vhd-geometry [^long total-sectors]
  (let [ts (min total-sectors (* 65535 16 255))]
    (if (> ts (* 65535 16 63))
      (let [spt 255 h 16] [(quot (quot ts spt) h) h spt])
      (let [spt 17
            cth (quot ts spt)
            h (max 4 (quot (+ cth 1023) 1024))]
        (if (or (>= cth (* h 1024)) (> h 16))
          (let [spt 31 h 16 cth (quot ts spt)]
            (if (>= cth (* h 1024))
              (let [spt 63 cth (quot ts spt)] [(quot cth 16) 16 spt])
              [(quot cth h) h spt]))
          [(quot cth h) h spt])))))

(defn vhd-footer ^bytes [^long size]
  (let [bb (.order (ByteBuffer/allocate 512) ByteOrder/BIG_ENDIAN)
        [cyl heads spt] (vhd-geometry (quot size 512))
        epoch2000 946684800]
    (.put bb (.getBytes "conectix" "US-ASCII"))
    (.putInt bb 2)
    (.putInt bb 0x00010000)
    (.putLong bb -1)
    (.putInt bb (unchecked-int (- (quot (System/currentTimeMillis) 1000) epoch2000)))
    (.put bb (.getBytes "bclj" "US-ASCII"))
    (.putInt bb 0x00010000)
    (.putInt bb 0x5769326B)
    (.putLong bb size)
    (.putLong bb size)
    (.putShort bb (unchecked-short cyl))
    (.put bb (unchecked-byte heads))
    (.put bb (unchecked-byte spt))
    (.putInt bb 2)
    (.putInt bb 0)
    (let [uuid (java.util.UUID/randomUUID)]
      (.putLong bb (.getMostSignificantBits uuid))
      (.putLong bb (.getLeastSignificantBits uuid)))
    (let [a (.array bb)
          sum (reduce (fn [^long s ^long x] (+ s (bit-and x 0xff))) 0 (seq a))
          ck (bit-and (bit-not sum) 0xffffffff)]
      (.putInt bb 64 (unchecked-int ck))
      a)))

(defn- images-in [items]
  (filterv (fn [it] (str/ends-with? (b/display (:path it)) ".img")) items))

(defn manifest-of [r k items]
  (when-let [it (first (filter #(= "imager.edn" (b/display (:path %))) items))]
    (let [bos (java.io.ByteArrayOutputStream.)]
      (doseq [c (:chunks it)] (.write bos ^bytes (arch/chunk-bytes r k c)))
      (edn/read-string (String. (.toByteArray bos) "UTF-8")))))

(defn pick-image [items want]
  (let [imgs (images-in items)]
    (cond
      want (or (first (filter #(= want (b/display (:path %))) imgs))
               (throw (ex-info (str "no image " want " in this archive")
                               {:have (mapv #(b/display (:path %)) imgs)})))
      (= 1 (count imgs)) (first imgs)
      (empty? imgs) (throw (ex-info "this archive holds no image" {}))
      :else (throw (ex-info "archive holds several images -- name one with --image"
                            {:have (mapv #(b/display (:path %)) imgs)})))))

(defn write-image!
  "Writes the image, or the byte range [from from+span), to sink."
  [r k item sink {:keys [skip-zeros progress from span]}]
  (let [total (arch/item-size item)
        from (long (or from 0))
        span (long (or span (- total from)))
        stop (+ from span)]
    (when (> stop total)
      (throw (ex-info "range runs past the end of the image" {:from from :span span :image total})))
    (loop [off 0 [c & more] (:chunks item) written 0 skipped 0 n 0]
      (if-not c
        (do (when progress (progress {:done span :of span :written written :skipped skipped}))
            {:length span :written written :skipped skipped :chunks n})
        (let [len (long (second c))
              end (+ off len)]
          (if (or (<= end from) (>= off stop))
            (recur end more written skipped n)
            (let [^bytes data (arch/chunk-bytes r k c)
                  len (alength data)
                  lo (max off from)
                  hi (min (+ off len) stop)
                  cut (- hi lo)
                  slice (if (and (= lo off) (= hi (+ off len)))
                          data
                          (java.util.Arrays/copyOfRange data (int (- lo off)) (int (- hi off))))
                  zero (and skip-zeros (image/zero-block? slice cut))]
              (when-not zero (w! sink (- lo from) slice cut))
              (when (and progress (zero? (mod n 64)))
                (progress {:done (- lo from) :of span :written written :skipped skipped}))
              (recur (+ off len) more
                     (if zero written (+ written cut))
                     (if zero (+ skipped cut) skipped)
                     (inc n)))))))))

(defn image-reader
  "Wahlfreier Zugriff auf ein Abbild im Archiv: (fn [off len] -> bytes)."
  ([r k item] (image-reader r k item 8))
  ([r k item ^long keep]
   (let [chunks (vec (:chunks item))
         n (count chunks)
         starts (long-array (inc n))
         _ (dotimes [i n]
             (aset starts (inc i) (+ (aget starts i) (long (second (nth chunks i))))))
         total (aget starts n)
         cache (atom [])
         fetch (fn [^long i]
                 (if-let [hit (some (fn [e] (when (= i (long (:i e))) e)) @cache)]
                   (do (swap! cache (fn [c] (vec (cons hit (remove #(= i (long (:i %))) c)))))
                       (:b hit))
                   (let [^bytes b (arch/chunk-bytes r k (nth chunks i))]
                     (swap! cache (fn [c] (vec (take keep (cons {:i i :b b} c)))))
                     b)))]
     (fn [^long off ^long len]
       (when (or (neg? off) (> (+ off len) total))
         (throw (ex-info "hinter dem Ende des Abbilds gelesen"
                         {:offset off :length len :image total})))
       (let [out (byte-array len)]
         (loop [at off into 0]
           (when (< into len)
             (let [p (java.util.Arrays/binarySearch starts 0 (inc n) at)
                   i (if (>= p 0) p (- (- p) 2))
                   ^bytes c (fetch i)
                   in-chunk (- at (aget starts i))
                   take- (min (- len into) (- (alength c) in-chunk))]
               (when-not (pos? take-)
                 (throw (ex-info "Block ist kuerzer als die Blockliste sagt"
                                 {:chunk i :offset at})))
               (System/arraycopy c (int in-chunk) out into (int take-))
               (recur (+ at take-) (+ into take-)))))
         out)))))

(defn partition-base
  "Wo eine Partition im Abbild anfaengt, und wie lang sie ist. Fuer ein Abbild,
   das nur einen Datentraeger traegt, ist das der Anfang."
  [im nm pn probe?]
  (let [ds (or (first (filter #(= nm (:image %)) (:sources im))) (first (:sources im)))
        parts (sort-by :number (:partitions ds))]
    (cond
      (not= :disk (:kind ds)) {:base 0 :length (:length ds) :letter (:letter ds)}

      pn (let [p (or (first (filter #(= (long pn) (long (:number %))) parts))
                     (throw (ex-info (str "no partition " pn " in this image")
                                     {:have (mapv :number parts)})))
               v (first (filter #(= (long (:offset p)) (long (:offset %))) (:volumes ds)))]
           {:base (:offset p) :length (:length p) :letter (:letter v) :partition (:number p)})

      :else
      (or (some (fn [p]
                  (let [v (first (filter #(= (long (:offset p)) (long (:offset %))) (:volumes ds)))]
                    (when (probe? (long (:offset p)))
                      {:base (:offset p) :length (:length p)
                       :letter (:letter v) :partition (:number p)})))
                parts)
          (throw (ex-info "no partition in this image carries a filesystem this can read"
                          {:have (mapv :number parts)}))))))

(defn with-table
  "The manifest with the partitions of image nm filled in from the image's own
   partition table, when imager.edn does not name them. That is the case for a
   whole disk imaged on Linux as file:/dev/sda: the table is in the first
   sectors all the same. A volume image (NTFS right at the start) stays as it is
   -- its boot sector ends in 55AA too and must not pass for a disk."
  [im nm read-at]
  (let [srcs (vec (:sources im))
        i (or (first (keep-indexed (fn [i s] (when (= nm (:image s)) i)) srcs)) 0)
        ds (get srcs i)]
    (if (or (nil? ds) (= :disk (:kind ds)) (ntfs/boot read-at))
      im
      (if-let [t (pt/read-table read-at)]
        (let [len (long (or (:length ds) Long/MAX_VALUE))
              parts (filterv #(<= (+ (long (:offset %)) (long (:length %))) len) (:parts t))]
          (if (seq parts)
            (assoc im :sources
                   (assoc srcs i (assoc ds :kind :disk :style (:style t) :partitions parts
                                        :volumes [] :head (:head t) :from-table true)))
            im))
        im))))

(defn open-image-volume
  "The NTFS volume inside an image in an archive, ready for lookup and reads."
  [r k items want pn]
  (let [item (pick-image items want)
        nm (b/display (:path item))
        read-at (image-reader r k item)
        im (with-table (or (manifest-of r k items)
                           (throw (ex-info "no imager.edn in this archive" {})))
             nm read-at)
        at (partition-base
            im nm pn
            (fn [^long base] (some? (ntfs/boot (fn [o l] (read-at (+ base o) l))))))
        base (long (:base at))
        vol (or (ntfs/open (fn [o l] (read-at (+ base o) l)))
                (throw (ex-info (str "no NTFS filesystem in " nm
                                     (when-let [n (:partition at)] (str ", partition " n))
                                     " -- ls and get read NTFS only")
                                {})))]
    (assoc at :vol vol :image nm)))
