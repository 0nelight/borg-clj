(ns borgclj.parttable
  "Reads the partition table at the start of a disk image: MBR with extended
   partitions, or GPT.

   On Windows, image writes the partitions of a disk into imager.edn, because
   the system tells it. On Linux it backs up a whole disk as a file
   (file:/dev/sda) and knows nothing of its partitions. The same facts are in
   the image itself, in its first sectors -- they are read from there, through
   the same read-at function the NTFS reader uses."
  (:import [java.nio ByteBuffer ByteOrder]))

(set! *warn-on-reflection* true)

(defn- le ^ByteBuffer [^bytes b] (.order (ByteBuffer/wrap b) ByteOrder/LITTLE_ENDIAN))

(defn- u8 ^long [^bytes b ^long off] (bit-and (long (aget b off)) 0xff))
(defn- u32 ^long [^bytes b ^long off] (bit-and (long (.getInt (le b) (int off))) 0xffffffff))
(defn- u64 ^long [^bytes b ^long off] (.getLong (le b) (int off)))

(defn- signed? [^bytes b]
  (and (>= (alength b) 512) (= 0x55 (u8 b 0x1FE)) (= 0xAA (u8 b 0x1FF))))

(defn- mbr-entries
  "The four entries of an MBR or EBR, empty ones left out."
  [^bytes b]
  (for [i (range 4)
        :let [o (+ 0x1BE (* 16 i))
              type (u8 b (+ o 4))
              lba (u32 b (+ o 8))
              n (u32 b (+ o 12))]
        :when (and (pos? type) (pos? n))]
    {:slot i :active (= 0x80 (u8 b o)) :mbr-type type :lba lba :sectors n}))

(defn- extended? [t] (contains? #{0x05 0x0F 0x85} t))

(defn- logicals
  "The chain of EBRs in an extended partition. Each EBR counts its logical
   partition from itself, and the link to the next one from the start of the
   extended partition."
  [read-at ^long ext-lba ^long sector]
  (loop [ebr ext-lba n 5 out [] seen #{}]
    (if (or (contains? seen ebr) (> n 128))
      out
      (let [^bytes b (read-at (* ebr sector) 512)]
        (if-not (signed? b)
          out
          (let [[p nxt] (mbr-entries b)
                out (cond-> out
                      p (conj {:number n :offset (* (+ ebr (long (:lba p))) sector)
                               :length (* (long (:sectors p)) sector)
                               :mbr-type (:mbr-type p) :active (:active p) :logical true}))]
            (if (and nxt (extended? (:mbr-type nxt)))
              (recur (+ ext-lba (long (:lba nxt))) (inc n) out (conj seen ebr))
              out)))))))

(defn- guid ^String [^bytes b ^long off]
  (let [bb (le b)
        d1 (bit-and (long (.getInt bb (int off))) 0xffffffff)
        d2 (bit-and (long (.getShort bb (int (+ off 4)))) 0xffff)
        d3 (bit-and (long (.getShort bb (int (+ off 6)))) 0xffff)
        tail (for [i (range 8)] (u8 b (+ off 8 i)))]
    (format "%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X"
            d1 d2 d3 (nth tail 0) (nth tail 1) (nth tail 2) (nth tail 3)
            (nth tail 4) (nth tail 5) (nth tail 6) (nth tail 7))))

(defn- utf16 ^String [^bytes b ^long off ^long len]
  (let [s (String. b (int off) (int len) "UTF-16LE")
        z (.indexOf s (int 0))]
    (if (neg? z) s (subs s 0 z))))

(defn- gpt [read-at ^long sector]
  (let [^bytes h (read-at sector 512)]
    (when (= "EFI PART" (String. h 0 8 "US-ASCII"))
      (let [lba (u64 h 72)
            n (min 256 (u32 h 80))
            size (u32 h 84)
            ^bytes t (read-at (* lba sector) (* n size))]
        {:style :gpt
         :parts (vec (for [i (range n)
                           :let [o (* i size)
                                 type (guid t o)]
                           :when (not= type "00000000-0000-0000-0000-000000000000")]
                       (let [first-lba (u64 t (+ o 32))
                             last-lba (u64 t (+ o 40))]
                         {:number (inc i)
                          :offset (* first-lba sector)
                          :length (* (inc (- last-lba first-lba)) sector)
                          :type-guid type
                          :label (not-empty (utf16 t (+ o 56) 72))})))}))))

(defn read-table
  "{:style :mbr|:gpt :parts [...] :head bytes-before-the-first-partition}, or nil
   when no partition table is at the start. :head is what Acronis calls \"MBR
   and Track 0\": everything before the first partition."
  ([read-at] (read-table read-at 512))
  ([read-at ^long sector]
   (let [^bytes b (try (read-at 0 512) (catch Throwable _ nil))]
     (when (and b (signed? b))
       (let [entries (mbr-entries b)
             protective (some #(= 0xEE (long (:mbr-type %))) entries)
             t (if protective
                 (gpt read-at sector)
                 (let [prim (for [e entries :when (not (extended? (:mbr-type e)))]
                              {:number (inc (long (:slot e)))
                               :offset (* (long (:lba e)) sector)
                               :length (* (long (:sectors e)) sector)
                               :mbr-type (:mbr-type e) :active (:active e)})
                       ext (first (filter #(extended? (:mbr-type %)) entries))]
                   {:style :mbr
                    :parts (vec (concat prim (when ext (logicals read-at (:lba ext) sector))))}))]
         ;; An NTFS boot sector ends in 55AA as well; without this check it
         ;; would pass for an MBR with nonsense in its table.
         (when (and t (seq (:parts t))
                    (every? #(pos? (long (:offset %))) (:parts t)))
           (let [parts (vec (sort-by :offset (:parts t)))]
             (assoc t :parts parts
                    :head (long (:offset (first parts)))))))))))

(def ^:private mbr-names
  {0x01 "FAT12" 0x04 "FAT16" 0x06 "FAT16" 0x07 "NTFS" 0x0B "FAT32" 0x0C "FAT32"
   0x0E "FAT16" 0x11 "FAT12" 0x12 "OEM" 0x17 "NTFS" 0x27 "Recovery"
   0x42 "LDM" 0x82 "Linux swap" 0x83 "Linux" 0x8E "Linux LVM" 0xA5 "FreeBSD"
   0xAF "HFS+" 0xEE "GPT" 0xEF "EFI" 0xFD "Linux RAID"})

(def ^:private gpt-names
  {"C12A7328-F81F-11D2-BA4B-00A0C93EC93B" "EFI System"
   "E3C9E316-0B5C-4DB8-817D-F92DF00215AE" "Microsoft reserved"
   "EBD0A0A2-B9E5-4433-87C0-68B6B72699C7" "Basic data"
   "DE94BBA4-06D1-4D40-A16A-BFD50179D6AC" "Windows Recovery"
   "0FC63DAF-8483-4772-8E79-3D69D8477DE4" "Linux"
   "0657FD6D-A4AB-43C4-84E5-0933C84B4F4F" "Linux swap"
   "E6D6D379-F507-44C2-A23C-238F2A3DF928" "Linux LVM"
   "21686148-6449-6E6F-744E-656564454649" "BIOS boot"})

(defn type-name
  "A readable name for the partition type, from the MBR byte or the GPT GUID."
  ^String [{:keys [mbr-type type-guid]}]
  (cond
    mbr-type (get mbr-names (long mbr-type) (format "Type %02Xh" (long mbr-type)))
    type-guid (get gpt-names (str type-guid) "GPT partition")
    :else ""))
