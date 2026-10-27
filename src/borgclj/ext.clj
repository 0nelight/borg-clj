(ns borgclj.ext
  "Belegungskarte und Dateilaeufe aus ext2/3/4 -- dieselbe Rolle wie
   borgclj.ntfs, rein rechnend ueber eine Lesefunktion."
  (:require [borgclj.source :as src]))

(def ^:const superblock-offset 1024)
(def ^:const magic 0xEF53)
(def ^:const extent-magic 0xF30A)

(def ^:const incompat-64bit   0x80)
(def ^:const incompat-extents 0x40)

(def ^:const bg-inode-uninit 0x1)
(def ^:const bg-block-uninit 0x2)

(def ^:const flag-extents 0x80000)
(def ^:const flag-inline-data 0x10000000)

(def ^:const mode-fmt-mask 0xF000)
(def ^:const mode-regular  0x8000)

;; --- little-endian Zugriffe -------------------------------------------------

(defn- u8 ^long [^bytes b ^long off]
  (bit-and (aget b (int off)) 0xFF))

(defn- u16 ^long [^bytes b ^long off]
  (bit-or (u8 b off) (bit-shift-left (u8 b (inc off)) 8)))

(defn- u32 ^long [^bytes b ^long off]
  (bit-or (u16 b off) (bit-shift-left (u16 b (+ off 2)) 16)))

;; --- Superblock -------------------------------------------------------------

(defn superblock
  "Der Superblock, oder nil."
  [read-at]
  (let [b (read-at superblock-offset 1024)]
    (when (= magic (u16 b 56))
      (let [incompat (u32 b 96)
            sixty-four? (pos? (bit-and incompat incompat-64bit))
            log-bs (u32 b 24)
            block-size (bit-shift-left 1024 log-bs)
            rev (u32 b 76)
            blocks (bit-or (u32 b 4)
                           (if sixty-four? (bit-shift-left (u32 b 336) 32) 0))]
        {:block-size block-size
         :blocks blocks
         :first-data-block (u32 b 20)
         :blocks-per-group (u32 b 32)
         :inodes-per-group (u32 b 40)
         :inodes (u32 b 0)
         :first-ino (if (zero? rev) 11 (u32 b 84))
         ;; vor rev 1 gab es das Feld nicht
         :inode-size (if (zero? rev) 128 (u16 b 88))
         :incompat incompat
         :extents? (pos? (bit-and incompat incompat-extents))
         :wide? sixty-four?
         :desc-size (if sixty-four? (max 64 (u16 b 254)) 32)
         :length (* blocks block-size)}))))

;; --- Gruppen-Deskriptoren ---------------------------------------------------

(defn- group-count ^long [{:keys [^long blocks ^long first-data-block ^long blocks-per-group]}]
  (let [usable (- blocks first-data-block)]
    (quot (+ usable (dec blocks-per-group)) blocks-per-group)))

(defn group-descs
  "Wo Block-Bitmap und Inode-Tabelle jeder Gruppe liegen."
  [read-at {:keys [^long block-size ^long first-data-block ^long desc-size wide?] :as sb}]
  (let [n (group-count sb)
        gdt-block (inc first-data-block)
        raw (read-at (* gdt-block block-size) (* n desc-size))]
    (mapv (fn [i]
            (let [o (* i desc-size)
                  hi (fn [^long at] (if wide? (bit-shift-left (u32 raw (+ o at)) 32) 0))]
              {:group i
               :block-bitmap (bit-or (u32 raw o) (hi 32))
               :inode-bitmap (bit-or (u32 raw (+ o 4)) (hi 36))
               :inode-table (bit-or (u32 raw (+ o 8)) (hi 40))
               :flags (u16 raw (+ o 18))}))
          (range n))))

;; --- Stufe 1: belegte Bloecke ----------------------------------------------

(defn- bitmap-runs
  "Laeufe gesetzter Bits als [block anzahl]; ganze Bytes am Stueck."
  [^bytes bm ^long base ^long limit]
  (let [n (min (long limit) (* 8 (alength bm)))]
    (persistent!
     (loop [i 0 start -1 acc (transient [])]
       (cond
         (>= i n)
         (if (neg? start) acc (conj! acc [(+ base start) (- n start)]))

         (and (zero? (rem i 8)) (<= (+ i 8) n) (neg? start)
              (zero? (aget bm (int (quot i 8)))))
         (recur (+ i 8) start acc)

         :else
         (let [set? (pos? (bit-and (aget bm (int (quot i 8)))
                                   (bit-shift-left 1 (rem i 8))))]
           (cond
             (and set? (neg? start)) (recur (inc i) i acc)
             (and (not set?) (not (neg? start)))
             (recur (inc i) -1 (conj! acc [(+ base start) (- i start)]))
             :else (recur (inc i) start acc))))))))

(defn- group-metadata-blocks
  "Was eine Gruppe in jedem Fall selbst belegt -- gilt bei BLOCK_UNINIT."
  [{:keys [block-bitmap inode-bitmap inode-table]}
   {:keys [^long inodes-per-group ^long inode-size ^long block-size]}]
  (let [itab-blocks (quot (+ (* inodes-per-group inode-size) (dec block-size)) block-size)]
    [[block-bitmap 1] [inode-bitmap 1] [inode-table itab-blocks]]))

(defn allocated-runs
  "Alle belegten Bloecke als [block anzahl]."
  [read-at {:keys [^long block-size ^long blocks ^long first-data-block
                   ^long blocks-per-group] :as sb} descs]
  (into []
        (mapcat
         (fn [{:keys [^long block-bitmap ^long flags] :as gd}]
           (let [base (+ first-data-block (* (long (:group gd)) blocks-per-group))
                 limit (min blocks-per-group (- blocks base))]
             (if (pos? (bit-and flags bg-block-uninit))
               (group-metadata-blocks gd sb)
               (bitmap-runs (read-at (* block-bitmap block-size) block-size) base limit))))
         descs)))

;; --- Stufe 2: Dateianfaenge -------------------------------------------------

(defn- extent-runs
  "Physische Laeufe aus einem ext4-Extent-Baum; depth>0 sind Indexknoten."
  [read-at ^long block-size ^bytes node ^long off]
  (when (= extent-magic (u16 node off))
    (let [entries (u16 node (+ off 2))
          depth (u16 node (+ off 6))]
      (into []
            (mapcat
             (fn [i]
               (let [e (+ off 12 (* i 12))]
                 (if (zero? depth)
                   (let [len (u16 node (+ e 4))
                         ;; >32768: angelegt, nie beschrieben
                         len (if (> len 32768) (- len 32768) len)
                         start (bit-or (u32 node (+ e 8))
                                       (bit-shift-left (u16 node (+ e 6)) 32))]
                     (when (pos? len) [[start len]]))
                   (let [leaf (bit-or (u32 node (+ e 4))
                                      (bit-shift-left (u16 node (+ e 8)) 32))
                         child (read-at (* leaf block-size) block-size)]
                     (extent-runs read-at block-size child 0)))))
             (range entries))))))

(defn- indirect-runs
  "Physische Laeufe aus den indirekten Bloecken von ext2/ext3."
  [read-at block-size ^bytes inode ib level]
  (letfn [(follow [^long blk ^long lvl]
            (if (zero? blk)
              []
              (if (zero? lvl)
                [[blk 1]]
                (let [b (read-at (* blk block-size) block-size)
                      n (quot block-size 4)]
                  (into [] (mapcat (fn [i]
                                     (let [nxt (u32 b (* i 4))]
                                       (when (pos? nxt) (follow nxt (dec lvl)))))
                                   (range n)))))))]
    (follow (u32 inode (+ ib (* level 4))) level)))

(defn- merge-runs
  [runs]
  (persistent!
   (reduce (fn [acc [^long s ^long n]]
             (let [c (count acc)
                   [^long ps ^long pn] (when (pos? c) (nth acc (dec c)))]
               (if (and ps (= (+ ps pn) s))
                 (assoc! acc (dec c) [ps (+ pn n)])
                 (conj! acc [s n]))))
           (transient []) runs)))

(defn- inode-runs
  [read-at {:keys [^long block-size]} ^bytes inode ^long off]
  (let [mode (u16 inode off)
        links (u16 inode (+ off 26))
        flags (u32 inode (+ off 32))
        ib (+ off 40)]
    (when (and (pos? links)
               (= mode-regular (bit-and mode mode-fmt-mask))
               ;; Inline-Data steht im Inode selbst
               (zero? (bit-and flags flag-inline-data)))
      (merge-runs
       (if (pos? (bit-and flags flag-extents))
         (extent-runs read-at block-size inode ib)
         (into (into [] (mapcat (fn [i] (let [b (u32 inode (+ ib (* i 4)))]
                                          (when (pos? b) [[b 1]])))
                                (range 12)))
               (mapcat (fn [lvl] (indirect-runs read-at block-size inode (+ ib 48) lvl))
                       [1 2 3])))))))

(defn file-run-starts
  "Die Bloecke, an denen ein Dateilauf beginnt -- die Schnitte."
  [read-at {:keys [^long block-size ^long inodes-per-group ^long inode-size
                   ^long inodes ^long first-ino] :as sb} descs]
  (let [per-read 64]
    (into []
          (mapcat
           (fn [{:keys [^long inode-table ^long inode-bitmap ^long flags ^long group]}]
             (when-not (pos? (bit-and flags bg-inode-uninit))
               (let [base-ino (inc (* group inodes-per-group))
                     ;; Die Tabelle waechst mit der Kapazitaet, nicht mit dem
                     ;; Inhalt -- die Bitmap erspart das Lesen leerer Bereiche
                     ibm (try (read-at (* inode-bitmap block-size) block-size)
                              (catch Throwable _ nil))
                     belegt? (fn [^long from ^long n]
                               (or (nil? ibm)
                                   (boolean
                                    (some (fn [^long i]
                                            (pos? (bit-and (aget ^bytes ibm (int (quot i 8)))
                                                           (bit-shift-left 1 (rem i 8)))))
                                          (range from (+ from n))))))]
                 (mapcat
                  (fn [chunk]
                    (let [first-idx (* chunk per-read)
                          n (min per-read (- inodes-per-group first-idx))]
                      (when (and (pos? n) (belegt? first-idx n))
                        (let [raw (read-at (+ (* inode-table block-size)
                                              (* first-idx inode-size))
                                           (* n inode-size))]
                          (mapcat
                           (fn [i]
                             (let [ino (+ base-ino first-idx i)]
                               ;; die ersten Inodes sind reserviert
                               (when (and (>= ino first-ino) (<= ino inodes))
                                 (keep (fn [[s _]] s)
                                       (inode-runs read-at sb raw (* i inode-size))))))
                           (range n))))))
                  (range (quot (+ inodes-per-group (dec per-read)) per-read))))))
           descs))))

;; --- Zusammenbau ------------------------------------------------------------

(defn probe
  "Die :extents und :cuts fuer eine Quelle, oder nil. Faellt :cuts aus, bleibt
   das Abbild richtig -- es dedupliziert nur schlechter."
  ([read-at] (probe read-at {}))
  ([read-at {:keys [cuts? sector] :or {cuts? true sector 512}}]
   (when-let [sb (superblock read-at)]
     (let [descs (group-descs read-at sb)
           bs (long (:block-size sb))
           length (long (:length sb))
           alloc (src/runs->extents (allocated-runs read-at sb descs) bs)
           ;; Superblock und Deskriptoren stehen nicht in jeder Bitmap
           head [[0 (min length (* 1024 1024))]]
           starts (when cuts?
                    (try (file-run-starts read-at sb descs)
                         (catch Throwable _ nil)))]
       {:info (assoc (dissoc sb :incompat)
                     :kind :ext
                     :type (if (:extents? sb) :ext4 :ext2-3)
                     :groups (count descs))
        :extents (src/normalize (concat head alloc) length sector)
        :cuts (mapv #(* (long %) bs) (sort (distinct starts)))
        :length length}))))

;; --- Quelle ueber einer Partition -------------------------------------------

(defrecord ExtSource [parent ^long base ^String name ^long length ^long sector
                      extents cuts info]
  src/Source
  (s-name [_] name)
  (s-length [_] length)
  (s-sector [_] sector)
  (s-extents [_] extents)
  (s-cuts [_] cuts)
  (s-read [_ off dst len] (src/s-read parent (+ base (long off)) dst len))
  (s-info [_] info)
  (s-close [_] nil))

(defn open-partition-source
  "Eine Quelle ueber eine ext-Partition, oder nil. align=false laesst die
   Inode-Tabellen aus: festes Raster, schlechteres Dedup."
  [parent base length ^String name
   {:keys [align sector] :or {align true sector 512}}]
  (let [base (long base)
        length (long length)
        read-at (fn [off len]
                  (let [b (byte-array len)]
                    (src/s-read parent (+ base (long off)) b len)
                    b))]
    (when-let [{:keys [extents cuts info] :as p} (probe read-at {:cuts? align :sector sector})]
      ;; Passt die Groesse nicht ins Fenster, gehoert die Karte zu etwas anderem
      (when (<= (long (:length p)) length)
        (->ExtSource parent base name length sector
                     (src/normalize (concat extents
                                            (when (< (long (:length p)) length)
                                              [[(:length p) (- length (long (:length p)))]]))
                                    length sector)
                     (src/prune-cuts cuts sector length)
                     (assoc info
                            :kind :ext-partition
                            :length length
                            :bitmap-driven true
                            :chunking (if (seq cuts) :run-relative :fixed)
                            :allocated-bytes (src/total extents)))))))
