(ns borgclj.ntfs
  "Belegungskarte, Dateilaeufe und einzelne Dateien aus NTFS -- roh gelesen,
   ohne Treiber und ohne mount. borgclj.win.mft leistet dasselbe ueber
   Windows-Aufrufe, die es von Linux aus nicht gibt."
  (:require [borgclj.source :as src]
            [clojure.string :as str]))

(def ^:const boot-magic "NTFS    ")

(def ^:const attr-attribute-list 0x20)
(def ^:const attr-file-name 0x30)
(def ^:const attr-data 0x80)
(def ^:const attr-index-root 0x90)
(def ^:const attr-index-allocation 0xA0)
(def ^:const attr-reparse-point 0xC0)
(def ^:const attr-end 0xFFFFFFFF)

;; Im Index, nicht 0x10 wie sonst.
(def ^:const fn-attr-index-present 0x10000000)

(def ^:const idx-has-subnode 0x01)
(def ^:const idx-last 0x02)

(def ^:const index-name "$I30")

(def ^:const attr-flag-compressed 0x0001)
(def ^:const attr-flag-encrypted  0x4000)

(def ^:const rec-flag-in-use   0x01)
(def ^:const rec-flag-directory 0x02)

(def ^:const rec-root 5)
(def ^:const rec-bitmap 6)

(def ^:const ns-dos 2)

(def default-excludes
  ["pagefile.sys"
   "hiberfil.sys"
   "swapfile.sys"
   "System Volume Information"
   "$Recycle.Bin"])

;; --- little-endian Zugriffe -------------------------------------------------

(defn- u8 ^long [^bytes b ^long off]
  (bit-and (aget b (int off)) 0xFF))

(defn- i8 ^long [^bytes b ^long off]
  (long (aget b (int off))))

(defn- u16 ^long [^bytes b ^long off]
  (bit-or (u8 b off) (bit-shift-left (u8 b (inc off)) 8)))

(defn- u32 ^long [^bytes b ^long off]
  (bit-or (u16 b off) (bit-shift-left (u16 b (+ off 2)) 16)))

(defn- u64 ^long [^bytes b ^long off]
  (bit-or (u32 b off) (bit-shift-left (u32 b (+ off 4)) 32)))

(defn- utf16 ^String [^bytes b ^long off ^long chars]
  (String. b (int off) (int (* 2 chars)) java.nio.charset.StandardCharsets/UTF_16LE))

;; --- Bootsektor -------------------------------------------------------------

(defn boot
  "Der Bootsektor, oder nil. :length zaehlt den Reserve-Bootsektor im letzten
   Sektor mit, total_sectors tut das nicht."
  [read-at]
  (let [b (try (read-at 0 512) (catch Throwable _ nil))]
    (when (and b (>= (alength ^bytes b) 512)
               (= boot-magic (String. ^bytes b 3 8 "US-ASCII"))
               (= 0x55 (u8 b 0x1FE)) (= 0xAA (u8 b 0x1FF)))
      (let [sector (u16 b 0x0B)
            spc-raw (u8 b 0x0D)
            ;; ueber 128: Zweierlogarithmus im Zweierkomplement
            spc (if (> spc-raw 128) (bit-shift-left 1 (- 256 spc-raw)) spc-raw)
            cluster (* sector spc)
            total (u64 b 0x28)
            rec-raw (i8 b 0x40)]
        (when (and (pos? sector) (zero? (rem sector 512)) (pos? spc) (pos? total)
                   (<= cluster (* 64 1024 1024)))
          {:sector-size sector
           :cluster-size cluster
           :total-sectors total
           :clusters (quot total spc)
           :length (* (inc total) sector)
           :mft-lcn (u64 b 0x30)
           :record-size (if (neg? rec-raw) (bit-shift-left 1 (- rec-raw)) (* rec-raw cluster))
           :serial (u64 b 0x48)})))))

;; --- MFT-Satz ---------------------------------------------------------------

(defn fixup!
  "Legt die Update-Sequence zurueck; false, wenn die Pruefzahl nicht passt.
   Die Schrittweite kommt aus dem Satz selbst, nicht aus der Sektorgroesse --
   ein 4Kn-Datentraeger sichert trotzdem alle 512 Byte."
  [^bytes b ^long size]
  (let [usa-off (u16 b 4)
        usa-cnt (u16 b 6)]
    (if-not (and (> usa-cnt 1) (pos? usa-off)
                 (<= (+ usa-off (* 2 usa-cnt)) size)
                 (zero? (rem size (dec usa-cnt))))
      false
      (let [stride (quot size (dec usa-cnt))
            usn (u16 b usa-off)]
        (if-not (and (>= stride 512) (zero? (bit-and stride (dec stride))))
          false
          (loop [i 1]
            (if (>= i usa-cnt)
              true
              (let [at (- (* i stride) 2)]
                (if-not (= usn (u16 b at))
                  false
                  (do (aset b (int at) (aget b (int (+ usa-off (* 2 i)))))
                      (aset b (int (inc at)) (aget b (int (+ usa-off (* 2 i) 1))))
                      (recur (inc i))))))))))))

(defn attributes
  "Die Attribute eines entfixten MFT-Satzes, oder nil."
  [^bytes rec ^long size]
  (when (and (>= size 48) (= 0x454C4946 (u32 rec 0)))   ; "FILE"
    (let [used (min size (u32 rec 0x18))]
      (loop [o (long (u16 rec 0x14)) acc []]
        (if (or (> (+ o 4) used) (= attr-end (u32 rec o)) (> (+ o 16) used))
          acc
          (let [alen (u32 rec (+ o 4))]
            (if (or (< alen 16) (not (zero? (rem alen 8))) (> (+ o alen) used))
              acc
              (let [non-res? (pos? (u8 rec (+ o 8)))
                    name-len (u8 rec (+ o 9))
                    name-off (u16 rec (+ o 10))
                    a (cond-> {:type (u32 rec o)
                               :offset o
                               :length alen
                               :non-resident? non-res?
                               :flags (u16 rec (+ o 12))
                               :name (if (pos? name-len) (utf16 rec (+ o name-off) name-len) "")}
                        non-res? (merge {:start-vcn (u64 rec (+ o 0x10))
                                         :last-vcn (u64 rec (+ o 0x18))
                                         :mp-offset (u16 rec (+ o 0x20))
                                         :alloc-size (u64 rec (+ o 0x28))
                                         :data-size (u64 rec (+ o 0x30))
                                         :init-size (u64 rec (+ o 0x38))})
                        (not non-res?) (merge {:value-length (u32 rec (+ o 0x10))
                                               :value-offset (u16 rec (+ o 0x14))}))]
                (recur (+ o alen) (conj acc a))))))))))

(defn- attr-of [attrs type name]
  (first (filter #(and (= type (:type %)) (= name (:name %))) attrs)))

;; --- Lauflisten (mapping pairs) --------------------------------------------

(defn decode-runlist
  "[{:lcn l :clusters n} ..]; :lcn nil ist ein Loch. Loecher zaehlen zur
   logischen Laenge -- ohne sie verrutscht der Inhalt dahinter."
  [^bytes b ^long off ^long limit]
  (loop [o off lcn 0 acc []]
    (if (>= o limit)
      acc
      (let [head (u8 b o)]
        (if (zero? head)
          acc
          (let [len-sz (bit-and head 0x0F)
                off-sz (bit-and (bit-shift-right head 4) 0x0F)]
            (if (or (zero? len-sz) (> len-sz 8) (> off-sz 8)
                    (> (+ o 1 len-sz off-sz) limit))
              acc
              (let [n (loop [i 0 v 0] (if (>= i len-sz) v
                                          (recur (inc i) (bit-or v (bit-shift-left (u8 b (+ o 1 i)) (* 8 i))))))
                    delta (when (pos? off-sz)
                            (loop [i 0 v 0]
                              (if (>= i off-sz)
                                (let [shift (- 64 (* 8 off-sz))]
                                  (bit-shift-right (bit-shift-left v shift) shift))
                                (recur (inc i) (bit-or v (bit-shift-left (u8 b (+ o 1 len-sz i)) (* 8 i)))))))
                    lcn (if delta (+ lcn (long delta)) lcn)
                    nxt (+ o 1 len-sz off-sz)]
                (cond
                  (not (pos? n)) acc
                  (nil? delta) (recur nxt lcn (conj acc {:lcn nil :clusters n}))
                  (neg? lcn) acc                            ; Lauf zeigt hinter den Anfang
                  :else (recur nxt lcn (conj acc {:lcn lcn :clusters n})))))))))))

(defn decode-runs
  "Nur die Laeufe, die auf der Platte stehen: [[lcn clusters] ..]."
  [^bytes b ^long off ^long limit]
  (into [] (keep (fn [{:keys [lcn clusters]}] (when lcn [lcn clusters])))
        (decode-runlist b off limit)))

(defn attr-runs
  [^bytes rec attr]
  (when (:non-resident? attr)
    (decode-runs rec (+ (long (:offset attr)) (long (:mp-offset attr)))
                 (+ (long (:offset attr)) (long (:length attr))))))

(defn attr-runlist
  [^bytes rec attr]
  (when (:non-resident? attr)
    (decode-runlist rec (+ (long (:offset attr)) (long (:mp-offset attr)))
                    (+ (long (:offset attr)) (long (:length attr))))))

;; --- Lauflisten adressieren -------------------------------------------------

(defn runs->reader
  "Laufliste -> (fn [logischer-versatz len] -> bytes)."
  [read-at runs ^long cluster]
  (let [idx (reduce (fn [{:keys [at v]} [lcn n]]
                      {:at (+ (long at) (* (long n) cluster))
                       :v (conj v {:from (long at)
                                   :to (+ (long at) (* (long n) cluster))
                                   :lcn (long lcn)})})
                    {:at 0 :v []} runs)
        spans (:v idx)
        total (long (:at idx))]
    (fn [^long off ^long len]
      (let [out (byte-array len)]
        (loop [at off into 0]
          (when (< into len)
            (let [want (- len into)
                  s (some (fn [s] (when (and (>= at (long (:from s))) (< at (long (:to s)))) s)) spans)]
              (when-not s
                (throw (ex-info "Laufliste deckt diesen Versatz nicht"
                                {:offset at :total total})))
              (let [take- (min want (- (long (:to s)) at))
                    disk (+ (* (long (:lcn s)) cluster) (- at (long (:from s))))
                    ^bytes chunk (read-at disk take-)]
                (System/arraycopy chunk 0 out into (int take-))
                (recur (+ at take-) (+ into take-))))))
        out))))

(defn- runs-length ^long [runs ^long cluster]
  (reduce (fn [^long a [_ n]] (+ a (* (long n) cluster))) 0 runs))

;; --- $MFT -------------------------------------------------------------------

(defn- read-record
  [read-at ^long off ^long size]
  (let [^bytes b (try (read-at off size) (catch Throwable _ nil))]
    (when (and b (= 0x454C4946 (u32 b 0)) (fixup! b size))
      b)))

(defn mft-runs
  "Die Laeufe der MFT selbst, aus Satz 0 -- dem einzigen, den der Bootsektor
   ohne die MFT auffindbar macht."
  [read-at {:keys [^long cluster-size ^long record-size ^long mft-lcn]}]
  (when-let [rec (read-record read-at (* mft-lcn cluster-size) record-size)]
    (let [attrs (attributes rec record-size)
          data (attr-of attrs attr-data "")
          runs (vec (attr-runs rec data))]
      (when (seq runs)
        ;; Weitere $DATA-Stuecke stehen in Folgesaetzen, die selbst in der MFT
        ;; liegen -- auffindbar nur ueber das Stueck aus Satz 0. So weit geht es.
        (if-let [al (attr-of attrs attr-attribute-list "")]
          (let [al-bytes (if (:non-resident? al)
                           (try ((runs->reader read-at (attr-runs rec al) cluster-size)
                                 0 (long (:data-size al)))
                                (catch Throwable _ nil))
                           (java.util.Arrays/copyOfRange rec
                                                         (int (+ (long (:offset al)) (long (:value-offset al))))
                                                         (int (+ (long (:offset al)) (long (:value-offset al))
                                                                 (long (:value-length al))))))]
            (if-not al-bytes
              runs
              (let [reader (runs->reader read-at runs cluster-size)
                    extra (loop [o 0 seen #{} acc []]
                            (let [^bytes ab al-bytes]
                              (if (> (+ o 26) (alength ab))
                                acc
                                (let [elen (u16 ab (+ o 4))
                                      etype (u32 ab o)
                                      ref (bit-and (u64 ab (+ o 16)) 0xFFFFFFFFFFFF)]
                                  (if (or (< elen 26) (> (+ o elen) (alength ab)))
                                    acc
                                    (if (or (not= etype attr-data) (zero? ref) (seen ref))
                                      (recur (+ o elen) (conj seen ref) acc)
                                      (let [child (try (let [^bytes c (reader (* ref record-size) record-size)]
                                                         (when (fixup! c record-size) c))
                                                       (catch Throwable _ nil))]
                                        (recur (+ o elen) (conj seen ref)
                                               (if-let [d (and child (attr-of (attributes child record-size) attr-data ""))]
                                                 (into acc (attr-runs child d))
                                                 acc)))))))))]
                (into runs extra))))
          runs)))))

;; --- Stufe 1: belegte Cluster ----------------------------------------------

(defn- bitmap-runs
  "Laeufe gesetzter Bits. Ganze 0x00- und 0xFF-Bytes am Stueck, bitweise nur
   die Raender."
  [^bytes bm ^long base ^long nbits ^long carry]
  (let [n (min nbits (* 8 (alength bm)))]
    (loop [i 0 start (long carry) acc []]
      (cond
        (>= i n) [acc start]

        (and (zero? (rem i 8)) (<= (+ i 8) n) (neg? start)
             (zero? (aget bm (int (quot i 8)))))
        (recur (+ i 8) start acc)

        (and (zero? (rem i 8)) (<= (+ i 8) n) (not (neg? start))
             (= -1 (aget bm (int (quot i 8)))))
        (recur (+ i 8) start acc)

        :else
        (let [set? (pos? (bit-and (aget bm (int (quot i 8)))
                                  (bit-shift-left 1 (rem i 8))))]
          (cond
            (and set? (neg? start)) (recur (inc i) (+ base i) acc)
            (and (not set?) (not (neg? start)))
            (recur (inc i) -1 (conj acc [start (- (+ base i) start)]))
            :else (recur (inc i) start acc)))))))

(defn allocated-runs
  "Alle belegten Cluster als [cluster anzahl], aus $Bitmap (MFT-Satz 6)."
  [read-at {:keys [^long cluster-size ^long record-size ^long clusters]} mftr]
  (let [mft-read (runs->reader read-at mftr cluster-size)
        rec (let [^bytes c (mft-read (* rec-bitmap record-size) record-size)]
              (when (fixup! c record-size) c))
        data (when rec (attr-of (attributes rec record-size) attr-data ""))]
    (when (and data (:non-resident? data))
      (let [runs (attr-runs rec data)
            bytes- (min (long (:data-size data)) (runs-length runs cluster-size))
            read-bits (runs->reader read-at runs cluster-size)
            step (* 1024 1024)]
        (loop [at 0 carry -1 acc []]
          (if (>= at bytes-)
            (if (neg? carry) acc (conj acc [carry (- clusters carry)]))
            (let [take- (min step (- bytes- at))
                  chunk (read-bits at take-)
                  base (* 8 at)
                  nbits (min (* 8 take-) (max 0 (- clusters base)))
                  [runs' carry'] (bitmap-runs chunk base nbits carry)]
              (recur (+ at take-) carry' (into acc runs')))))))))

;; --- Stufe 2: Dateilaeufe ---------------------------------------------------

(defn- best-file-name
  "Name und Verzeichnis eines Satzes. Von zwei $FILE_NAME gilt der lange."
  [^bytes rec attrs]
  (reduce (fn [best a]
            (if-not (and (= attr-file-name (:type a)) (not (:non-resident? a)))
              best
              (let [v (+ (long (:offset a)) (long (:value-offset a)))
                    nsp (u8 rec (+ v 0x41))
                    nm (utf16 rec (+ v 0x42) (u8 rec (+ v 0x40)))
                    cand {:parent (bit-and (u64 rec v) 0xFFFFFFFFFFFF) :name nm :namespace nsp}]
                (if (or (nil? best) (and (= ns-dos (:namespace best)) (not= ns-dos nsp)))
                  cand best))))
          nil attrs))

(defn- exclude-set
  "Ausgeschlossene Wurzelverzeichnisse samt allem darunter."
  [dirs patterns]
  (let [pats (set (map str/lower-case patterns))
        roots (into #{} (keep (fn [[frn {:keys [parent name]}]]
                                (when (and (= rec-root (long parent)) (pats (str/lower-case name)))
                                  frn))
                              dirs))]
    (loop [out roots]
      (let [more (into out (keep (fn [[frn {:keys [parent]}]]
                                   (when (and (out parent) (not (out frn))) frn))
                                 dirs))]
        (if (= (count more) (count out)) out (recur more))))))

(defn scan
  "Ein Durchgang durch die MFT: Schnitte und ausgeschlossene Byte-Bereiche.
   Behalten wird nur, was min-run erreicht -- sonst kostet eine Million Dateien
   eine Million Eintraege. Laeufe in Folgesaetzen bleiben ungesehen; das kostet
   Schnitte, nicht Richtigkeit."
  [read-at {:keys [^long cluster-size ^long record-size]} mftr
   {:keys [min-run excludes progress]}]
  (let [min-run (long (or min-run (* 256 1024)))
        excludes (or excludes default-excludes)
        mft-read (runs->reader read-at mftr cluster-size)
        total-bytes (runs-length mftr cluster-size)
        n-records (quot total-bytes record-size)
        per-read (max 1 (quot (* 1024 1024) record-size))
        report (let [last (volatile! 0)]
                 (fn [m] (when progress
                           (let [now (System/nanoTime)]
                             (when (>= (- now (long @last)) 1000000000)
                               (vreset! last now) (progress m))))))]
    (loop [i 0 dirs (transient {}) big (transient []) seen 0]
      (if (>= i n-records)
        (let [dirs (persistent! dirs)
              big (persistent! big)
              dropped (exclude-set dirs excludes)
              pats (set (map str/lower-case excludes))
              drop? (fn [{:keys [^long parent ^String name]}]
                      (or (dropped parent)
                          (and (= rec-root parent) (pats (str/lower-case name)))))
              {:keys [cuts excluded]}
              (reduce (fn [acc f]
                        (if (drop? f)
                          (update acc :excluded into
                                  (map (fn [[lcn n]] [(* (long lcn) cluster-size)
                                                      (* (long n) cluster-size)]))
                                  (:runs f))
                          (update acc :cuts into
                                  (comp (filter (fn [[_ n]] (>= (* (long n) cluster-size) min-run)))
                                        (map (fn [[lcn _]] (* (long lcn) cluster-size))))
                                  (:runs f))))
                      {:cuts [] :excluded []} big)]
          {:cuts cuts :excluded excluded
           :records n-records :in-use seen
           :dirs (count dirs) :files (count big)
           :excluded-files (count (filter drop? big))})
        (let [take- (min per-read (- n-records i))
              ^bytes buf (try (mft-read (* i record-size) (* take- record-size))
                              (catch Throwable _ nil))]
          (if-not buf
            (recur (+ i take-) dirs big seen)
            (let [[dirs big seen]
                  (loop [j 0 dirs dirs big big seen seen]
                    (if (>= j take-)
                      [dirs big seen]
                      (let [^bytes rec (java.util.Arrays/copyOfRange
                                        buf (int (* j record-size)) (int (* (inc j) record-size)))]
                        (if-not (and (= 0x454C4946 (u32 rec 0))
                                     (fixup! rec record-size)
                                     (pos? (bit-and (u16 rec 0x16) rec-flag-in-use)))
                          (recur (inc j) dirs big seen)
                          (let [frn (+ i j)
                                dir? (pos? (bit-and (u16 rec 0x16) rec-flag-directory))
                                attrs (attributes rec record-size)
                                fname (best-file-name rec attrs)]
                            (cond
                              (nil? fname) (recur (inc j) dirs big (inc (long seen)))

                              dir? (recur (inc j)
                                          (assoc! dirs frn (select-keys fname [:parent :name]))
                                          big (inc (long seen)))

                              :else
                              (let [d (attr-of attrs attr-data "")]
                                (if-not (and d (:non-resident? d)
                                             (zero? (bit-and (long (:flags d))
                                                             (bit-or attr-flag-compressed
                                                                     attr-flag-encrypted)))
                                             (>= (long (:alloc-size d)) min-run))
                                  (recur (inc j) dirs big (inc (long seen)))
                                  (recur (inc j) dirs
                                         (conj! big (assoc (select-keys fname [:parent :name])
                                                           :runs (attr-runs rec d)))
                                         (inc (long seen)))))))))))]
              (report {:phase :ntfs-mft :records (+ i take-) :records-total n-records
                       :percent (* 100.0 (/ (double (+ i take-)) (double (max 1 n-records))))})
              (recur (+ i take-) dirs big seen))))))))

;; --- Zusammenbau ------------------------------------------------------------

(defn probe
  "Die :extents und :cuts fuer eine Quelle, oder nil. Faellt der MFT-Durchgang aus,
   bleibt das Abbild richtig -- es dedupliziert nur schlechter."
  ([read-at] (probe read-at {}))
  ([read-at {:keys [cuts? sector excludes min-run progress]
             :or {cuts? true}}]
   (when-let [bt (boot read-at)]
     (when-let [mftr (mft-runs read-at bt)]
       (let [excludes (or excludes default-excludes)
             min-run (long (or min-run (* 256 1024)))
             cluster (long (:cluster-size bt))
             length (long (:length bt))
             sector (long (or sector (:sector-size bt)))
             alloc (src/runs->extents (or (allocated-runs read-at bt mftr) []) cluster)]
         (when (seq alloc)
           (let [;; $Boot und der Reservesektor stehen nicht zwingend in der Bitmap
                 head [[0 (min length (* 1024 1024))]]
                 tail [[(- length sector) sector]]
                 area (min length (* (long (:clusters bt)) cluster))
                 rest- (when (< area (- length sector)) [[area (- length sector area)]])
                 sc (when cuts?
                      (try (scan read-at bt mftr {:min-run min-run :excludes excludes
                                                  :progress progress})
                           (catch Throwable _ nil)))
                 extents (src/normalize (concat head alloc rest- tail) length sector)
                 extents (if (seq (:excluded sc))
                           (src/subtract extents (:excluded sc) length sector)
                           extents)
                 high (reduce (fn [^long a [o l]] (max a (+ (long o) (long l)))) 0 alloc)]
             {:info {:kind :ntfs
                     :ntfs true
                     :sector-size (:sector-size bt)
                     :cluster-size cluster
                     :total-clusters (:clusters bt)
                     :record-size (:record-size bt)
                     :mft-fragments (count mftr)
                     :serial (format "%016x" (long (:serial bt)))
                     :mft-records (:records sc)
                     :mft-files (:files sc)
                     :mft-dirs (:dirs sc)
                     :excluded-files (:excluded-files sc)}
              :extents extents
              :cuts (vec (sort (distinct (:cuts sc))))
              :excluded (:excluded sc)
              :highest-allocated high
              :allocated (src/total (src/normalize alloc length sector))
              :length length})))))))

;; --- Quelle ueber einer Partition -------------------------------------------

(defrecord NtfsSource [parent ^long base ^String name ^long length ^long sector
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
  "Eine Quelle ueber eine NTFS-Partition, oder nil. align=false laesst den
   MFT-Durchgang aus: festes Raster, Auslagerungsdatei inbegriffen."
  [parent base length ^String name
   {:keys [align sector excludes min-run progress letter] :or {align true}}]
  (let [base (long base)
        length (long length)
        sector (long (or sector 512))
        min-run (long (or min-run (* 256 1024)))
        read-at (fn [off len]
                  (let [b (byte-array len)]
                    (src/s-read parent (+ base (long off)) b len)
                    b))]
    (when-let [{:keys [extents cuts info highest-allocated] :as p}
               (probe read-at {:cuts? align :sector sector :min-run min-run
                               :excludes excludes :progress progress})]
      ;; Passt die Groesse nicht ins Fenster, gehoert die Karte zu etwas anderem
      (when (<= (long (:length p)) length)
        (->NtfsSource parent base (or letter name) length sector
                      (src/normalize (concat extents
                                             (when (< (long (:length p)) length)
                                               [[(:length p) (- length (long (:length p)))]]))
                                     length sector)
                      (src/prune-cuts cuts (max sector (quot min-run 4)) length)
                      (assoc info
                             :kind :ntfs-partition
                             :letter letter
                             :length length
                             :bitmap-driven true
                             :chunking (if (seq cuts) :run-relative :fixed)
                             :highest-allocated-byte highest-allocated
                             :excluded-bytes (when (seq (:excluded p))
                                               (src/total (src/normalize (:excluded p) length sector)))
                             :allocated-bytes (src/total extents)))))))

;; --- Einzelne Dateien ------------------------------------------------------
;; Nur der Weg zu EINER Datei, Verzeichnis um Verzeichnis. Die MFT wird nicht
;; durchlaufen -- das ist der Unterschied zwischen "geht" und "geht ueber SSH".

(defn open
  "Ein Lesegriff auf einzelne Dateien, oder nil."
  [read-at]
  (when-let [bt (boot read-at)]
    (when-let [mftr (mft-runs read-at bt)]
      (let [rs (long (:record-size bt))
            mread (runs->reader read-at mftr (long (:cluster-size bt)))]
        {:read-at read-at
         :boot bt
         :record-size rs
         :record (fn [^long frn]
                   (let [^bytes c (try (mread (* frn rs) rs) (catch Throwable _ nil))]
                     (when (and c (= 0x454C4946 (u32 c 0)) (fixup! c rs)
                                (pos? (bit-and (u16 c 0x16) rec-flag-in-use)))
                       c)))}))))

(defn- index-node
  "Eintraege eines Indexknotens ab dem INDEX_HEADER bei hdr. Der letzte Eintrag
   traegt keinen Namen, wohl aber den rechten Unterknoten."
  [^bytes b ^long hdr ^long limit]
  (if (> (+ hdr 16) limit)
    []
    (let [stop (min limit (+ hdr (u32 b (+ hdr 4))))]
      (loop [o (+ hdr (u32 b hdr)) out []]
        (if (> (+ o 16) stop)
          out
          (let [elen (u16 b (+ o 8))
                flags (u16 b (+ o 12))]
            (if (or (< elen 16) (> (+ o elen) stop))
              out
              (let [sub (when (pos? (bit-and flags idx-has-subnode)) (u64 b (+ o elen -8)))
                    v (+ o 16)
                    ent (when (and (zero? (bit-and flags idx-last)) (<= (+ v 0x42) stop))
                          (let [nlen (u8 b (+ v 0x40))]
                            (when (<= (+ v 0x42 (* 2 nlen)) stop)
                              {:frn (bit-and (u64 b o) 0xFFFFFFFFFFFF)
                               :name (utf16 b (+ v 0x42) nlen)
                               :namespace (u8 b (+ v 0x41))
                               :dir? (pos? (bit-and (u32 b (+ v 0x38)) fn-attr-index-present))
                               :size (u64 b (+ v 0x30))})))
                    out (cond-> out sub (conj {:subnode sub}) ent (conj ent))]
                (if (pos? (bit-and flags idx-last))
                  out
                  (recur (+ o elen) out))))))))))

(defn dir-entries
  "Eintraege eines Verzeichnisses, sortiert, ohne 8.3-Zweitnamen; nil sonst.
   :size kommt aus dem Index und kann fuer eine offene Datei zu klein sein --
   read-file! liefert die richtige Laenge."
  [vol ^long frn]
  (when-let [^bytes rec ((:record vol) frn)]
    (let [rs (long (:record-size vol))
          attrs (attributes rec rs)
          root (attr-of attrs attr-index-root index-name)]
      (when (and root (not (:non-resident? root)))
        (let [cluster (long (:cluster-size (:boot vol)))
              sector (long (:sector-size (:boot vol)))
              v (+ (long (:offset root)) (long (:value-offset root)))
              ibs (u32 rec (+ v 8))
              alloc (attr-of attrs attr-index-allocation index-name)
              ia (when alloc (runs->reader (:read-at vol) (attr-runs rec alloc) cluster))
              ;; VCN zaehlt in Clustern, bei kleinen Bloecken in Sektoren
              unit (if (>= ibs cluster) cluster sector)
              block (fn [^long vcn]
                      (when ia
                        (let [^bytes c (try (ia (* vcn unit) ibs) (catch Throwable _ nil))]
                          (when (and c (= 0x58444E49 (u32 c 0)) (fixup! c ibs)) c))))]
          (loop [todo (index-node rec (+ v 16) (+ (long (:offset root)) (long (:length root))))
                 seen #{} out []]
            (if-let [n (first todo)]
              (if-let [vcn (:subnode n)]
                (if (seen vcn)
                  (recur (rest todo) seen out)
                  (recur (concat (when-let [^bytes c (block (long vcn))] (index-node c 0x18 ibs))
                                 (rest todo))
                         (conj seen vcn) out))
                (recur (rest todo) seen (conj out n)))
              (->> out
                   (remove #(= ns-dos (long (:namespace %))))
                   ;; "." zeigt auf das Verzeichnis selbst und ist kein Kind
                   (remove #(= frn (long (:frn %))))
                   (sort-by :name)
                   vec))))))))

(defn lookup
  "Der Eintrag zu einem Pfad, oder nil. Gross-/Kleinschreibung zaehlt nicht."
  [vol ^String path]
  (let [parts (remove str/blank? (str/split (or path "") #"[\\/]+"))]
    (if (empty? parts)
      {:frn rec-root :name "" :dir? true}
      (loop [frn rec-root [c & more] parts]
        (when-let [es (dir-entries vol frn)]
          (when-let [hit (or (first (filter #(= c (:name %)) es))
                             (first (filter #(.equalsIgnoreCase ^String c ^String (:name %)) es)))]
            (if (empty? more)
              hit
              (when (:dir? hit) (recur (long (:frn hit)) more)))))))))

(defn file-info
  "Groesse und Art aus dem Satz selbst. :readable? false + :why, wenn der Inhalt
   nicht im Klartext auf der Platte steht."
  [vol ^long frn]
  (when-let [^bytes rec ((:record vol) frn)]
    (let [rs (long (:record-size vol))
          attrs (attributes rec rs)
          d (attr-of attrs attr-data "")
          wof (attr-of attrs attr-data "WofCompressedData")
          dir? (pos? (bit-and (u16 rec 0x16) rec-flag-directory))
          flags (long (:flags d 0))
          size (cond dir? 0
                     (nil? d) 0
                     (:non-resident? d) (long (:data-size d))
                     :else (long (:value-length d)))
          why (cond
                dir? "ein Verzeichnis hat keinen Datenstrom"
                (nil? d) "dieser Satz hat keinen unbenannten Datenstrom"
                (pos? (bit-and flags attr-flag-encrypted))
                "mit EFS verschluesselt -- der Schluessel steckt im Windows-Profil"
                (pos? (bit-and flags attr-flag-compressed))
                "NTFS-komprimiert -- dieser Leser gibt nur unkomprimierte Daten heraus"
                wof "von Windows ueberlagert komprimiert (WOF/CompactOS), der Inhalt steht in einem eigenen Datenstrom")]
      {:frn frn :dir? dir? :size size
       :resident? (and d (not (:non-resident? d)))
       :readable? (nil? why)
       :why why})))

(defn- write-zeros! [^java.io.OutputStream os ^long n]
  (let [z (byte-array (min n (* 256 1024)))]
    (loop [left n]
      (when (pos? left)
        (let [t (min left (alength z))]
          (.write os z 0 (int t))
          (recur (- left t)))))))

(defn- data-segments
  "Laufliste als [versatz laenge lcn], auf size beschnitten; lcn nil ist ein Loch."
  [rl ^long cluster ^long size]
  (loop [[r & more] rl at 0 out []]
    (if (or (nil? r) (>= at size))
      out
      (let [n (min (- size at) (* (long (:clusters r)) cluster))]
        (recur more (+ at n) (conj out [at n (:lcn r)]))))))

(defn read-file!
  "Schreibt den Inhalt nach os, liefert die Bytezahl, wirft nach file-info.
   Loecher und alles hinter initialized_size sind Null, auch wenn dort alte
   Bytes liegen -- wer sie ausgibt, gibt fremde Daten aus."
  [vol ^long frn ^java.io.OutputStream os]
  (let [info (file-info vol frn)]
    (when-not info (throw (ex-info "diesen Satz gibt es nicht" {:frn frn})))
    (when-not (:readable? info)
      (throw (ex-info (:why info) {:frn frn})))
    (let [^bytes rec ((:record vol) frn)
          rs (long (:record-size vol))
          d (attr-of (attributes rec rs) attr-data "")
          size (long (:size info))]
      (if-not (:non-resident? d)
        (do (.write os rec (int (+ (long (:offset d)) (long (:value-offset d)))) (int size))
            size)
        (let [cluster (long (:cluster-size (:boot vol)))
              init (long (:init-size d))
              buf (* 1024 1024)]
          (doseq [[at n lcn] (data-segments (attr-runlist rec d) cluster size)]
            (loop [off 0]
              (when (< off (long n))
                (let [want (min (- (long n) off) buf)
                      logical (+ (long at) off)]
                  (if (or (nil? lcn) (>= logical init))
                    (write-zeros! os want)
                    (let [t (min want (- init logical))
                          ^bytes c ((:read-at vol) (+ (* (long lcn) cluster) off) t)]
                      (.write os c 0 (int t))
                      (when (< t want) (write-zeros! os (- want t)))))
                  (recur (+ off want))))))
          size)))))
