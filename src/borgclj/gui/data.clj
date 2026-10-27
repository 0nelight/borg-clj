(ns borgclj.gui.data
  "What the wizards read from a repository: which backups exist, when they were
   made, and what an image holds -- disk, partitions, file systems, labels,
   used space. All of it straight from the archive, as in the web interface; no
   verb is run and its output read back."
  (:require [borgclj.archive :as arch]
            [borgclj.bytes :as b]
            [borgclj.key :as key]
            [borgclj.ntfs :as ntfs]
            [borgclj.parttable :as pt]
            [borgclj.restore :as restore]
            [borgclj.store :as store]
            [borgclj.ui.config :as config]
            [clojure.string :as str])
  (:import [java.time LocalDateTime ZoneId ZoneOffset]))

(set! *warn-on-reflection* true)

(defn- with-store [cfg f]
  (let [k (key/plaintext-key)
        st (store/open (config/location cfg nil) (config/store-opts cfg))]
    (try (f st k (arch/manifest st k)) (finally (store/close st)))))

(defn- text ^String [x] (if (string? x) x (b/display x)))

(defn- local-time
  "borg stores times in UTC without a zone. They are shown in local time."
  ^LocalDateTime [x]
  (when x
    (try
      (-> (LocalDateTime/parse (subs (text x) 0 (min 19 (count (text x)))))
          (.atOffset ZoneOffset/UTC)
          (.atZoneSameInstant (ZoneId/systemDefault))
          (.toLocalDateTime))
      (catch Exception _ nil))))

(defn series-of
  "The backup series an archive belongs to: its name without the date and time
   at the end. That is what Acronis shows as one backup with several versions;
   in borg every version is an archive of its own."
  ^String [^String nm]
  (or (second (re-matches #"(.+?)[-_. ]*\d{4}-\d{2}-\d{2}.*" nm)) nm))

(defn archives
  "All archives, oldest first: {:name :time :series}."
  [cfg]
  (with-store cfg
    (fn [_ _ m]
      (->> (:archives m)
           (map (fn [[nm v]]
                  (let [n (b/display (b/key->str nm))]
                    {:name n :series (series-of n) :time (local-time (:time v))})))
           (sort-by (juxt :time :name))
           vec))))

;; --- inside an image -----------------------------------------------------------

(defn- ntfs-facts
  "Label and used bytes of an NTFS file system, or nil. The label is attribute
   0x60 of $Volume (MFT record 3)."
  [read-at]
  (when-let [v (ntfs/open read-at)]
    (let [bt (:boot v)
          rs (long (:record-size v))
          ^bytes rec ((:record v) 3)
          attrs (when rec (ntfs/attributes rec rs))
          vn (first (filter #(and (= 0x60 (long (:type %))) (not (:non-resident? %))) attrs))
          label (when vn
                  (String. rec (int (+ (long (:offset vn)) (long (:value-offset vn))))
                           (int (:value-length vn)) "UTF-16LE"))
          used (try
                 (when-let [mftr (ntfs/mft-runs read-at bt)]
                   (* (long (:cluster-size bt))
                      (reduce + 0 (map (fn [[_ n]] (long n))
                                       (ntfs/allocated-runs read-at bt mftr)))))
                 (catch Throwable _ nil))]
      {:fs "NTFS" :label (not-empty label) :used used})))

(defn- fs-facts
  "Which file system starts at read-at, as far as a signature tells."
  [read-at]
  (or (ntfs-facts read-at)
      (let [^bytes b (try (read-at 0 2048) (catch Throwable _ nil))
            s (fn [o n] (when b (String. b (int o) (int n) "US-ASCII")))]
        (cond
          (nil? b) nil
          (= "FAT32   " (s 0x52 8)) {:fs "FAT32"}
          (#{"FAT16   " "FAT12   "} (s 0x36 8)) {:fs (str/trim (s 0x36 8))}
          (= "EXFAT   " (s 3 8)) {:fs "exFAT"}
          (and (= 0x53 (bit-and (long (aget b 1080)) 0xff))
               (= 0xEF (bit-and (long (aget b 1081)) 0xff))) {:fs "ext4"}
          :else nil))))

(defn- flags
  "Partition flags as keywords; the window turns them into text."
  [p style]
  (cond-> []
    (= :mbr style) (conj (if (:logical p) :log :pri))
    (:active p) (conj :act)))

(defn- image-layout
  "An image as a disk with partitions, or as a single volume."
  [r k item im]
  (let [nm (b/display (:path item))
        read-at (restore/image-reader r k item)
        im (restore/with-table im nm read-at)
        ds (or (first (filter #(= nm (:image %)) (:sources im))) (first (:sources im)) {})
        total (arch/item-size item)]
    (if (= :disk (:kind ds))
      {:image nm
       :kind :disk
       :length (or (:length ds) total)
       :style (:style ds)
       :head (or (:head ds) (some->> (:partitions ds) (map :offset) seq (apply min)))
       :parts
       (vec (for [p (sort-by :offset (:partitions ds))]
              (let [v (first (filter #(= (long (:offset p)) (long (:offset %))) (:volumes ds)))
                    base (long (:offset p))
                    facts (fs-facts (fn [^long o ^long l] (read-at (+ base o) l)))]
                {:number (:number p)
                 :offset base
                 :length (:length p)
                 :letter (:letter v)
                 :label (or (:label facts) (:label p))
                 :fs (or (:fs facts) (pt/type-name p))
                 :used (or (:used facts) (:allocated-bytes v))
                 :flags (flags p (:style ds))
                 :type (pt/type-name p)})))}
      (let [facts (fs-facts read-at)]
        {:image nm
         :kind :volume
         :length total
         :parts [{:number nil :offset 0 :length total :letter (:letter ds)
                  :label (:label facts) :fs (:fs facts)
                  :used (or (:used facts) (:allocated-bytes ds)) :flags [] :type ""}]}))))

(defn layout
  "The images of an archive with their partitions."
  [cfg archive]
  (with-store cfg
    (fn [r k m]
      (let [items (arch/items r k (arch/archive r k m archive))
            im (or (restore/manifest-of r k items) {:sources []})
            imgs (filter #(str/ends-with? (b/display (:path %)) ".img") items)]
        (mapv #(image-layout r k % im) imgs)))))

(defn entries
  "One directory inside an image, for recovering files."
  [cfg {:keys [archive image partition path]}]
  (with-store cfg
    (fn [r k m]
      (let [items (arch/items r k (arch/archive r k m archive))
            where (restore/open-image-volume r k items (not-empty image) partition)
            vol (:vol where)
            hit (or (ntfs/lookup vol path)
                    (throw (ex-info (str "Nicht in diesem Abbild: " path) {})))]
        (->> (ntfs/dir-entries vol (long (:frn hit)))
             (remove #(str/starts-with? (str (:name %)) "$"))
             (sort-by (juxt (comp not :dir?) #(str/lower-case (str (:name %)))))
             (mapv (fn [e] {:name (:name e) :dir? (:dir? e)
                            :size (when-not (:dir? e) (:size e))})))))))
