(ns borgclj.win.ioctl
  (:require [borgclj.win.ffm :as ffm])
  (:import [java.nio ByteBuffer ByteOrder]))

(def ^:const disk-get-drive-geometry-ex 0x000700A0)
(def ^:const disk-get-drive-layout-ex   0x00070050)
(def ^:const disk-get-length-info       0x0007405C)
(def ^:const volume-get-disk-extents    0x00560000)
(def ^:const fsctl-get-ntfs-volume-data 0x00090064)
(def ^:const fsctl-get-volume-bitmap    0x0009006F)
(def ^:const fsctl-allow-extended-dasd  0x00090083)
(def ^:const fsctl-lock-volume          0x00090018)
(def ^:const fsctl-unlock-volume        0x0009001C)
(def ^:const fsctl-dismount-volume      0x00090020)
(def ^:const fsctl-query-usn-journal    0x000900F4)
(def ^:const fsctl-read-usn-journal     0x000900BB)
(def ^:const fsctl-get-retrieval-ptrs   0x00090073)
(def ^:const fsctl-enum-usn-data        0x000900B3)

(defn buf ^ByteBuffer [^bytes b]
  (.order (ByteBuffer/wrap b) ByteOrder/LITTLE_ENDIAN))

(defn- u32 ^long [^ByteBuffer b ^long off] (bit-and (long (.getInt b (int off))) 0xffffffff))
(defn- i64 ^long [^ByteBuffer b ^long off] (.getLong b (int off)))

(defn- le64 ^bytes [^long v]
  (.array (.putLong (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN) v)))

(defn geometry [dev]
  (let [b (buf (:out (ffm/ioctl dev disk-get-drive-geometry-ex nil 32)))]
    {:sector-size (u32 b 20) :length (i64 b 24)}))

(defn length-info ^long [dev]
  (i64 (buf (:out (ffm/ioctl dev disk-get-length-info nil 8))) 0))

(defn- guid ^String [^ByteBuffer b ^long off]
  (format "%08x-%04x-%04x-%02x%02x-%02x%02x%02x%02x%02x%02x"
          (u32 b off) (bit-and (.getShort b (int (+ off 4))) 0xffff)
          (bit-and (.getShort b (int (+ off 6))) 0xffff)
          (bit-and (.get b (int (+ off 8))) 0xff) (bit-and (.get b (int (+ off 9))) 0xff)
          (bit-and (.get b (int (+ off 10))) 0xff) (bit-and (.get b (int (+ off 11))) 0xff)
          (bit-and (.get b (int (+ off 12))) 0xff) (bit-and (.get b (int (+ off 13))) 0xff)
          (bit-and (.get b (int (+ off 14))) 0xff) (bit-and (.get b (int (+ off 15))) 0xff)))

(def ^:const entry-size 144)
(def ^:const entries-at 48)

(defn drive-layout [dev]
  (loop [cap (+ entries-at (* 16 entry-size))]
    (let [{:keys [error out]} (ffm/ioctl dev disk-get-drive-layout-ex nil cap
                                         :tolerate #{ffm/error-more-data 122})]
      (if error
        (if (> cap (+ entries-at (* 512 entry-size)))
          (throw (ex-info "partition table too large" {}))
          (recur (* 2 cap)))
        (let [b (buf out)
              style (u32 b 0)
              n (u32 b 4)]
          (if (> (+ entries-at (* n entry-size)) cap)
            (recur (+ entries-at (* (inc n) entry-size)))
            {:style (case (int style) 0 :mbr 1 :gpt :raw)
             :mbr-signature (when (zero? style) (u32 b 8))
             :disk-id (when (= 1 style) (guid b 8))
             :partitions
             (vec (for [i (range n)
                        :let [o (+ entries-at (* i entry-size))
                              st (u32 b o)
                              start (i64 b (+ o 8))
                              len (i64 b (+ o 16))]
                        :when (pos? len)]
                    (cond-> {:number (u32 b (+ o 24)) :offset start :length len
                             :style (case (int st) 0 :mbr 1 :gpt :raw)}
                      (zero? st) (assoc :mbr-type (bit-and (.get b (int (+ o 32))) 0xff)
                                        :boot (pos? (bit-and (.get b (int (+ o 33))) 0xff)))
                      (= 1 st) (assoc :type-guid (guid b (+ o 32))
                                      :part-guid (guid b (+ o 48))
                                      :attributes (i64 b (+ o 64))
                                      :label (let [n (loop [j 0] (if (or (>= j 36)
                                                                         (zero? (.getShort b (int (+ o 72 (* 2 j))))))
                                                                   j (recur (inc j))))]
                                               (String. out (int (+ o 72)) (int (* 2 n))
                                                        java.nio.charset.StandardCharsets/UTF_16LE))))))}))))))

(defn disk-extents [dev]
  (let [{:keys [error out]} (ffm/ioctl dev volume-get-disk-extents nil (+ 8 (* 8 24))
                                       :tolerate #{ffm/error-more-data 1 50 87})]
    (when-not error
      (let [b (buf out)
            n (u32 b 0)]
        (vec (for [i (range (min n 8))
                   :let [o (+ 8 (* i 24))]]
               {:disk (u32 b o) :offset (i64 b (+ o 8)) :length (i64 b (+ o 16))}))))))

(defn ntfs-volume-data [dev]
  (let [{:keys [error out]} (ffm/ioctl dev fsctl-get-ntfs-volume-data nil 128
                                       :tolerate #{ffm/error-invalid-function ffm/error-not-supported 1 50 87})]
    (when-not error
      (let [b (buf out)]
        {:sectors (i64 b 8) :total-clusters (i64 b 16) :free-clusters (i64 b 24)
         :sector-size (u32 b 40) :cluster-size (u32 b 44)}))))

(defn allow-extended-dasd! [dev]
  (ffm/ioctl dev fsctl-allow-extended-dasd nil 0
             :tolerate #{ffm/error-invalid-function ffm/error-not-supported 1 50 87 5}))

(defn lock-volume! [dev]
  (:error (ffm/ioctl dev fsctl-lock-volume nil 0 :tolerate #{5 32 1 50})))

(defn dismount-volume! [dev]
  (:error (ffm/ioctl dev fsctl-dismount-volume nil 0 :tolerate #{5 32 1 50})))

(defn allocation-runs
  "Allocated cluster runs [[start-lcn count] ...] from the NTFS allocation
   bitmap."
  [dev]
  (let [probe (ffm/ioctl dev fsctl-get-volume-bitmap (le64 0) 32
                         :tolerate #{ffm/error-more-data ffm/error-invalid-function
                                     ffm/error-not-supported 1 50 87})]
    (when-not (and (:error probe) (not= ffm/error-more-data (:error probe)))
      (let [chunk (* 1024 1024)]
        (loop [lcn 0 acc (transient []) run nil]
          (let [{:keys [error out]} (ffm/ioctl dev fsctl-get-volume-bitmap (le64 lcn) (+ 16 chunk)
                                               :tolerate #{ffm/error-more-data})
                b (buf out)
                start (i64 b 0)
                total (i64 b 8)
                bits (min (* 8 chunk) (- total (- lcn start)))
                base (- lcn start)]
            (let [[acc run]
                  (loop [i 0 acc acc run run]
                    (if (>= i bits)
                      [acc run]
                      (let [idx (+ base i)
                            on (pos? (bit-and (aget ^bytes out (int (+ 16 (quot idx 8))))
                                              (bit-shift-left 1 (int (mod idx 8)))))
                            lc (+ start idx)]
                        (cond
                          (and on (nil? run)) (recur (inc i) acc [lc 1])
                          (and on run) (recur (inc i) acc [(nth run 0) (inc (long (nth run 1)))])
                          (and (not on) run) (recur (inc i) (conj! acc run) nil)
                          :else (recur (inc i) acc nil)))))
                  next-lcn (+ start base bits)]
              (if (or (not error) (>= next-lcn (+ start total)) (zero? bits))
                (persistent! (if run (conj! acc run) acc))
                (recur next-lcn acc run)))))))))

(defn usn-journal [dev]
  (let [{:keys [error out]} (ffm/ioctl dev fsctl-query-usn-journal nil 80
                                       :tolerate #{ffm/error-invalid-function ffm/error-not-supported
                                                   1 50 87 1179 1180 1181})]
    (when-not error
      (let [b (buf out)]
        {:journal-id (i64 b 0) :first-usn (i64 b 8) :next-usn (i64 b 16)
         :lowest-valid-usn (i64 b 24)}))))
