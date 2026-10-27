(ns borgclj.win.volume
  (:require [borgclj.ext :as ext]
            [borgclj.source :as src]
            [borgclj.win.ffm :as ffm]
            [borgclj.win.ioctl :as io]
            [borgclj.ntfs :as ntfs]
            [borgclj.win.mft :as mft]
            [borgclj.restore :as restore]
            [clojure.string :as str]))

(def default-excludes mft/default-excludes)

(defn letter->device ^String [^String letter]
  (str "\\\\.\\" (str/upper-case (str/replace letter #"[\\/:]+$" "")) ":"))

(defn disk->device ^String [n] (str "\\\\.\\PhysicalDrive" n))

(defrecord DevSource [dev ^String name ^long length ^long sector extents cuts info]
  src/Source
  (s-name [_] name)
  (s-length [_] length)
  (s-sector [_] sector)
  (s-extents [_] extents)
  (s-cuts [_] cuts)
  (s-read [_ off dst len] (ffm/read-at! dev off dst len))
  (s-info [_] info)
  (s-close [_] (.close ^java.lang.AutoCloseable dev)))

(defn- volume-length ^long [dev fallback]
  (try (io/length-info dev) (catch Exception _ (long (or fallback 0)))))

(defn probe-volume
  "Read-only facts about a mounted volume, or nil when it cannot be opened."
  [^String letter]
  (try
    (with-open [d (ffm/open (letter->device letter))]
      (let [nd (io/ntfs-volume-data d)
            ex (io/disk-extents d)]
        {:letter (str/upper-case (str/replace letter #":$" ""))
         :length (volume-length d 0)
         :sector-size (or (:sector-size nd) 512)
         :cluster-size (:cluster-size nd)
         :ntfs (boolean nd)
         :total-clusters (:total-clusters nd)
         :free-clusters (:free-clusters nd)
         :extents ex
         :usn (io/usn-journal d)}))
    (catch Exception e {:letter letter :error (ex-message e)})))

(defn volumes []
  (mapv probe-volume (ffm/logical-drives)))

(defn probe-disk [n]
  (try
    (with-open [d (ffm/open (disk->device n))]
      (let [g (io/geometry d)]
        (merge {:disk n :length (:length g) :sector-size (:sector-size g)}
               (try (io/drive-layout d) (catch Exception _ {:style :raw :partitions []})))))
    (catch Exception _ nil)))

(defn disks
  ([] (disks 16))
  ([limit] (vec (keep probe-disk (range limit)))))

(defn- fs-extents
  "Byte extents that must actually be read for a filesystem volume: the boot
   area, every allocated cluster, and whatever the cluster area does not
   cover (the NTFS backup boot sector lives in the volume's last sector)."
  [dev ^long length ^long sector nd]
  (let [cluster (long (or (:cluster-size nd) 0))
        runs (when (pos? cluster) (try (io/allocation-runs dev) (catch Exception _ nil)))]
    (if-not runs
      {:extents (src/normalize [[0 length]] length sector) :bitmap false}
      (let [alloc (src/runs->extents runs cluster)
            head [[0 (min length (* 1024 1024))]]
            area (min length (* (long (or (:total-clusters nd) 0)) cluster))
            tail (when (< area length) [[area (- length area)]])
            high (reduce (fn [^long a [o l]] (max a (+ (long o) (long l)))) 0 alloc)]
        {:extents (src/normalize (concat head alloc tail) length sector)
         :bitmap true :highest-allocated high}))))

(defn open-volume-source
  "A Source over one volume. device is either a drive letter or a VSS device path."
  [^String device {:keys [letter raw align excludes min-run progress]
                   :or {align true min-run (* 256 1024)}}]
  (let [d (ffm/open device)]
    (try
      (io/allow-extended-dasd! d)
      (let [nd (io/ntfs-volume-data d)
            sector (long (or (:sector-size nd) 512))
            length (volume-length d 0)
            _ (when-not (pos? length) (throw (ex-info (str "cannot size " device) {})))
            {:keys [extents bitmap highest-allocated]}
            (if raw
              {:extents (src/normalize [[0 length]] length sector) :bitmap false}
              (fs-extents d length sector nd))
            scan (when (and (not raw) nd (:cluster-size nd) (or align (seq excludes)))
                   (try (mft/scan d {:cluster-size (:cluster-size nd)
                                     :min-run min-run
                                     :excludes (or excludes mft/default-excludes)
                                     :progress (when progress
                                                 (fn [m] (progress (assoc m :volume (or letter device)))))})
                        (catch Throwable _ nil)))
            dropped (:excluded scan)
            extents (if (seq dropped) (src/subtract extents dropped length sector) extents)
            cuts (if align (src/prune-cuts (:cuts scan) (max sector (quot min-run 4)) length) [])]
        (->DevSource d (or letter device) length sector extents cuts
                     {:kind :volume :device device :letter letter
                      :sector-size sector :length length
                      :cluster-size (:cluster-size nd) :ntfs (boolean nd)
                      :total-clusters (:total-clusters nd) :free-clusters (:free-clusters nd)
                      :bitmap-driven bitmap
                      :highest-allocated-byte highest-allocated
                      :chunking (if (seq cuts) :run-relative :fixed)
                      :mft-files (:files scan)
                      :mft-runs (:runs scan)
                      :excluded-bytes (when (seq dropped) (src/total (src/normalize dropped length sector)))
                      :allocated-bytes (src/total extents)
                      :usn (io/usn-journal d)}))
      (catch Throwable t (.close ^java.lang.AutoCloseable d) (throw t)))))

(defn- volume-for-partition [vols disk part]
  (first (filter (fn [v]
                   (some (fn [e] (and (= (long (:disk e)) (long disk))
                                      (= (long (:offset e)) (long (:offset part)))))
                         (:extents v)))
                 vols)))

(defn open-disk-source
  "A Source over one physical disk. Partitions whose volume was snapshotted are
   read from the snapshot; everything else is read from the disk itself."
  [n {:keys [snapshot-devices raw align excludes min-run progress] :or {align true}}]
  (let [d (ffm/open (disk->device n))]
    (try
      (let [g (io/geometry d)
            sector (long (:sector-size g))
            length (long (:length g))
            layout (try (io/drive-layout d) (catch Exception _ {:style :raw :partitions []}))
            vols (if raw [] (try (volumes) (catch Exception _ [])))
            disk-src (->DevSource d (disk->device n) length sector nil nil {})
            subs (when-not raw
                   (keep (fn [p]
                           (let [v (volume-for-partition vols n p)
                                 dev (get snapshot-devices (:letter v))]
                             (or
                              (when (and v (or dev (:ntfs v)))
                                (try
                                  (let [s (open-volume-source (or dev (letter->device (str (:letter v) ":")))
                                                              (cond-> {:letter (:letter v) :align align}
                                                                excludes (assoc :excludes excludes)
                                                                min-run (assoc :min-run min-run)
                                                                progress (assoc :progress progress)))]
                                    (if (= (src/s-length s) (long (:length p)))
                                      {:part p :src s}
                                      (do (src/s-close s) nil)))
                                  (catch Exception _ nil)))
                               ;; Was Windows nicht als Laufwerk fuehrt, ist deshalb nicht
                              (let [nm (str "part" (:number p))
                                    args {:align align :sector sector :excludes excludes
                                          :min-run min-run :progress progress}
                                    off (long (:offset p)) len (long (:length p))]
                                (when-let [s (or (try (ntfs/open-partition-source disk-src off len nm args)
                                                      (catch Exception _ nil))
                                                 (try (ext/open-partition-source disk-src off len nm args)
                                                      (catch Exception _ nil)))]
                                  {:part p :src s})))))
                         (:partitions layout)))
            covered (sort-by :base
                             (map (fn [{:keys [part src]}]
                                    {:base (long (:offset part)) :size (long (:length part))
                                     :inner 0 :src src :own true :fs true :letter (src/s-name src)})
                                  subs))
            raw-part (fn [at size] {:base at :size size :inner at :src disk-src :own true})
            parts (loop [at 0 [c & more] covered acc []]
                    (if (nil? c)
                      (if (< at length) (conj acc (raw-part at (- length at))) acc)
                      (recur (+ (long (:base c)) (long (:size c))) more
                             (cond-> acc
                               (< at (long (:base c))) (conj (raw-part at (- (long (:base c)) at)))
                               true (conj c)))))
            extents (src/normalize
                     (mapcat (fn [p]
                               (if (:fs p)
                                 (map (fn [[o l]] [(+ (long (:base p)) (long o)) l])
                                      (src/s-extents (:src p)))
                                 [[(:base p) (:size p)]]))
                             parts)
                     length sector)]
        (src/->CompositeSource
         (disk->device n) length sector extents
         (src/prune-cuts (mapcat (fn [p]
                                   (when (:fs p)
                                     (map #(+ (long (:base p)) (long %)) (src/s-cuts (:src p)))))
                                 parts)
                         sector length)
         parts
         {:kind :disk :disk n :sector-size sector :length length
          :style (:style layout) :partitions (:partitions layout)
          :mbr-signature (:mbr-signature layout) :disk-id (:disk-id layout)
          :snapshotted (vec (keep :letter covered))
          :volumes (mapv (fn [{:keys [part src]}]
                           (let [i (src/s-info src)]
                             {:offset (:offset part) :length (:length part)
                              :letter (:letter i) :cluster-size (:cluster-size i)
                              :allocated-bytes (:allocated-bytes i)
                              :highest-allocated-byte (:highest-allocated-byte i)}))
                         subs)
          :bitmap-driven (boolean (seq covered))
          :allocated-bytes (src/total extents)}))
      (catch Throwable t (.close ^java.lang.AutoCloseable d) (throw t)))))

(defrecord DeviceSink [dev ^String name ^long size]
  restore/Sink
  (w! [_ off buf len] (ffm/write-at! dev off buf len))
  (sink-size [_] size)
  (set-size! [_ _] nil)
  (sink-close [_] (ffm/flush! dev) (.close ^java.lang.AutoCloseable dev)))

(defn system-disk []
  (let [sd (or (System/getenv "SystemDrive") "C:")]
    (some-> (probe-volume sd) :extents first :disk)))

(defn parse-target [^String t]
  (cond
    (re-matches #"(?i)disk:\d+" t) {:kind :disk :disk (parse-long (subs t 5))}
    (re-matches #"(?i)\\\\[.?]\\physicaldrive\d+" t) {:kind :disk :disk (parse-long (re-find #"\d+$" t))}
    (re-matches #"(?i)[a-z]:\\?" t) {:kind :volume :letter (str/upper-case (subs t 0 1))}
    (re-matches #"(?i)\\\\[.?]\\[a-z]:" t) {:kind :volume :letter (str/upper-case (subs t 4 5))}
    :else {:kind :file :path t}))

(defn open-device-sink
  [target {:keys [force]}]
  (let [{:keys [kind disk letter]} target
        sysdisk (try (system-disk) (catch Exception _ nil))]
    (case kind
      :disk (do
              (when (and (= (long (or disk -1)) (long (or sysdisk -2))) (not force))
                (throw (ex-info (str "PhysicalDrive" disk " carries the running system -- pass --force to write it anyway")
                                {:disk disk})))
              (doseq [v (volumes)
                      :when (some #(= (long (:disk %)) (long disk)) (:extents v))]
                (try (with-open [d (ffm/open (letter->device (:letter v)) {:write true})]
                       (io/lock-volume! d) (io/dismount-volume! d))
                     (catch Exception _ nil)))
              (let [d (ffm/open (disk->device disk) {:write true :share 0})]
                (io/allow-extended-dasd! d)
                (->DeviceSink d (disk->device disk) (io/length-info d))))
      :volume (do
                (when (and (= (str/upper-case (or (System/getenv "SystemDrive") "C:"))
                              (str letter ":"))
                           (not force))
                  (throw (ex-info (str letter ": is the running system volume -- restore it from recovery media")
                                  {:letter letter})))
                (let [d (ffm/open (letter->device letter) {:write true :share 0})]
                  (io/lock-volume! d)
                  (io/dismount-volume! d)
                  (io/allow-extended-dasd! d)
                  (->DeviceSink d (letter->device letter) (io/length-info d))))
      (throw (ex-info (str "not a device target: " target) {})))))
