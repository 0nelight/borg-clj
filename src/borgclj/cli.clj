(ns borgclj.cli
  (:require [borgclj.ext :as ext]
            [borgclj.archive :as arch]
            [borgclj.create :as create]
            [borgclj.image :as image]
            [borgclj.remote :as remote]
            [borgclj.restore :as restore]
            [borgclj.bytes :as b]
            [borgclj.chunker :as chunker]
            [borgclj.key :as key]
            [borgclj.log :as log]
            [borgclj.ntfs :as ntfs]
            [borgclj.platform :as plat]
            [borgclj.source :as src]
            [borgclj.store :as store]
            [borgclj.gui :as gui]
            [borgclj.ui.server :as ui]
            [borgclj.win.volume :as wv]
            [borgclj.win.vss :as vss]
            [clojure.string :as str])
  (:import [java.nio.file CopyOption Files OpenOption Path
            StandardCopyOption StandardOpenOption]))

(defn- path ^Path [^String s] (Path/of s (into-array String [])))

(defn- split-location [^String s]
  (let [[r a] (str/split s #"::" 2)]
    {:repo r :archive a}))

(defn- open-store
  ([loc opts] (open-store loc opts {}))
  ([loc opts store-opts]
   (let [k (key/plaintext-key)
         st (store/open loc opts store-opts)]
     [st k (arch/manifest st k)])))

(defn parse-size ^long [^String s]
  (if-let [[_ n u] (re-matches #"(?i)(\d+)\s*([kmg]?)b?" (str/trim s))]
    (* (parse-long n) (case (str/lower-case u) "k" 1024 "m" (* 1024 1024) "g" (* 1024 1024 1024) 1))
    (throw (ex-info (str "not a size: " s) {}))))

(defn stop-watch
  "Has someone asked the run to stop? Polled a few times a second, not per
   block -- in the free tail a stat call would cost more than the block."
  [^Path p]
  (if-not p
    (constantly false)
    (let [checked (volatile! 0)
          seen (volatile! false)]
      (fn []
        (when-not @seen
          (let [now (System/nanoTime)]
            (when (> (- now (long @checked)) 200000000)
              (vreset! checked now)
              (when (Files/exists p (into-array java.nio.file.LinkOption []))
                (vreset! seen true)))))
        @seen))))

(defn check-block
  "borg serve refuses anything over MAX_DATA_SIZE, and a block grows by three
   bytes on the way out. Unchecked, that refusal arrives a quarter of an hour
   into a run that was never going to work. Zero loops forever."
  ^long [^long b]
  (let [ceiling (- remote/max-data-size 3)]
    (cond
      (not (pos? b))
      (throw (ex-info "--block must be greater than zero" {:block b}))

      (> b ceiling)
      (throw (ex-info (format "--block %d is more than borg keeps in one object -- at most %d bytes (%s)"
                              b ceiling (log/human ceiling))
                      {:block b :max ceiling}))

      :else b)))

(defn cmd-list [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)]
    (try
      (if archive
        (doseq [it (arch/items r k (arch/archive r k m archive))]
          (let [d (arch/describe it)]
            (println (format "%-9s %10s %s%s"
                             (name (:type d)) (or (:size d) "") (:path d)
                             (if (:target d) (str " -> " (:target d)) "")))))
        (doseq [nm (arch/archive-names m)] (println nm)))
      (finally (store/close r)))))

(defn cmd-info [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)]
    (try
      (doseq [[kk v] (sort (store/describe r))]
        (println (format "%-18s %s" (name kk) v)))
      (println (format "%-18s %s" "manifest-version" (:version m)))
      (println (format "%-18s %s" "archives" (str/join " " (arch/archive-names m))))
      (when archive
        (let [a (arch/archive r k m archive)
              its (arch/items r k a)]
          (println (format "%-18s %s" "archive" (b/display (:name a))))
          (println (format "%-18s %s" "items" (count its)))
          (println (format "%-18s %s" "bytes" (reduce + 0 (map arch/item-size its))))
          (when-let [im (restore/manifest-of r k its)]
            (println (format "%-18s %s" "image-block-size" (:block-size im)))
            (doseq [s (:sources im)]
              (println (format "%-18s %s %s of %s allocated%s"
                               "image" (:image s) (log/human (long (or (:allocated-bytes s) 0)))
                               (log/human (long (or (:length s) 0)))
                               (if (:bitmap-driven s) "" " (raw, no bitmap)")))))))
      (finally (store/close r)))))

(defn cmd-extract [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)
        dest (path (or (get opts "--dest") "."))]
    (try
      (let [n (reduce (fn [acc it] (arch/extract-item! r k dest it) (inc acc))
                      0 (arch/items r k (arch/archive r k m archive)))]
        (println (format "extracted %d items to %s" n (str dest))))
      (finally (store/close r)))))

(defn cmd-chunk [{:keys [args]}]
  (let [p (path (first args))]
    (doseq [c (chunker/chunks p 0)]
      (println (b/hex (b/sha256 c)) (alength ^bytes c)))))

(defn cmd-verify [{:keys [args opts]}]
  (let [{:keys [repo]} (split-location (first args))
        [r k _] (open-store repo opts)
        bad (atom 0)
        ids (store/list-ids r)]
    (try
      (doseq [id ids
              :let [hx (b/hex id)]]
        (try (key/decrypt k (when-not (= hx (b/hex arch/manifest-id)) id) (store/get-object r id))
             (catch Exception e (swap! bad inc) (println "BAD" hx (ex-message e)))))
      (println (format "%d objects, %d bad" (count ids) @bad))
      (when (pos? @bad) (throw (ex-info "verify failed" {:bad @bad})))
      (finally (store/close r)))))

(defn cmd-platform [_]
  (doseq [[k v] (sort (plat/capabilities))]
    (println (format "%-16s %s" (name k) (if (coll? v) (str/join " " v) v)))))

(defn cmd-create [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        sources (mapv path (rest args))
        [conn rpath] (store/connect repo opts)
        k (key/plaintext-key (get opts "--compression"))]
    (when (empty? sources) (throw (ex-info "usage: borgi create REPO::ARCHIVE PATH..." {})))
    (try
      (let [repo-id (b/hex (remote/open! conn rpath :create (contains? opts "--init")))
            _ (when (contains? opts "--init") (create/init-repo! conn k))
            res (create/create! conn k archive sources (when-not (contains? opts "--no-cache") repo-id))]
        (println (format "archive %s: %d items, %d files (%d read, %d from cache), %d bytes (%d stored), %d chunks in repo"
                         (:archive res) (:items res) (:nfiles res) (:read res) (:reused res)
                         (:size res) (:csize res) (:chunks res))))
      (finally (remote/close conn)))))

(defn- linux? []
  (str/starts-with? (str (System/getProperty "os.name")) "Linux"))

(defn- parse-source [^String s]
  (cond
    (= "all" (str/lower-case s)) {:kind :all}
    (str/starts-with? s "file:") {:kind :file :path (subs s 5)}
    (re-matches #"(?i)disk:\d+" s) {:kind :disk :disk (parse-long (subs s 5))}
    (re-matches #"(?i)\\\\[.?]\\physicaldrive\d+" s) {:kind :disk :disk (parse-long (re-find #"\d+$" s))}
    (re-matches #"(?i)[a-z]:?\\?" s) {:kind :volume :letter (str/upper-case (subs s 0 1))}
    :else (throw (ex-info (str "not a source: " s
                               " -- use C:, disk:0, \\\\.\\PhysicalDrive0, all or file:PATH")
                          {}))))

(defn- expand-sources [specs]
  (vec (mapcat (fn [sp]
                 (if (= :all (:kind sp))
                   (let [vols (wv/volumes)]
                     (mapv (fn [v] {:kind :volume :letter (:letter v)})
                           (filter #(and (:ntfs %) (pos? (long (:length % 0)))) vols)))
                   [sp]))
               specs)))

(defn- volumes-to-snapshot [specs]
  (let [vols (delay (wv/volumes))]
    (vec (distinct
          (mapcat (fn [sp]
                    (case (:kind sp)
                      :volume [(:letter sp)]
                      :disk (keep (fn [v]
                                    (when (and (:ntfs v)
                                               (some #(= (long (:disk %)) (long (:disk sp))) (:extents v)))
                                      (:letter v)))
                                  @vols)
                      []))
                  specs)))))

(defn- open-sources [specs snapshot opts]
  (let [devs (:devices snapshot)
        common (select-keys opts [:raw :align :excludes :min-run :progress])]
    (mapv (fn [sp]
            (case (:kind sp)
              :file (let [f (src/file-source (path (:path sp)))]
                      ;; Traegt die Datei ein lesbares Dateisystem, kommt die
                      ;; Belegungskarte von dort statt den freien Teil mitzulesen
                      (or (when-not (:raw opts)
                            (let [args {:align (get opts :align true)
                                        :sector (src/s-sector f)
                                        :min-run (:min-run opts (* 256 1024))
                                        :excludes (:excludes opts)
                                        :progress (:progress opts)}]
                              (or (try (ntfs/open-partition-source
                                        f 0 (src/s-length f) (src/s-name f) args)
                                       (catch Exception _ nil))
                                  (try (ext/open-partition-source
                                        f 0 (src/s-length f) (src/s-name f) args)
                                       (catch Exception _ nil)))))
                          f))
              :volume (wv/open-volume-source (or (get devs (:letter sp))
                                                 (wv/letter->device (:letter sp)))
                                             (assoc common :letter (:letter sp)))
              :disk (wv/open-disk-source (:disk sp) (assoc common :snapshot-devices devs))))
          specs)))

(defn- write-status!
  "One move, so a reader never catches half a line."
  [^Path p m]
  (let [tmp (.resolveSibling p (str (.getFileName p) ".tmp"))]
    (Files/write tmp (.getBytes (pr-str m) "UTF-8")
                 (into-array OpenOption [StandardOpenOption/CREATE
                                         StandardOpenOption/TRUNCATE_EXISTING
                                         StandardOpenOption/WRITE]))
    (Files/move tmp p (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                              StandardCopyOption/ATOMIC_MOVE]))))

(defn- status!
  "Progress, for whoever is watching: the --status file, and the interface when
   it runs the verb in its own process. Errors are swallowed -- a status file
   must never fail a backup."
  [^Path p m]
  (when-let [f log/*progress*] (f m))
  (when p
    (try
      (write-status! p (assoc m :pid (.pid (java.lang.ProcessHandle/current))
                              :updated (log/stamp)))
      (catch Exception _ nil))))

(defn- scan-reporter
  "The MFT scan is the long silent stretch between the snapshot and the first
   block. This gives it a voice: the directory walk knows its total up front, so
   it reports a real percentage rather than a number that merely grows."
  [^Path status quiet]
  (fn [m]
    (status! status m)
    (when-not quiet
      (log/line
       (case (:phase m)
         :mft-records (log/fmt "mft              %9d records" (:records m))
         :mft-dirs (log/fmt "mft       %5.1f%%  %d/%d dirs, %d files, %d runs"
                            (:percent m) (:dirs m) (:dirs-total m) (:files m) (:runs m))
         (pr-str m))))))

(defn- source-detail ^String [s]
  (let [i (src/s-info s)]
    (str/join ", "
              (remove nil?
                      [(str (src/s-name s) " " (log/human (long (src/s-length s))))
                       (when-let [a (:allocated-bytes i)] (str (log/human (long a)) " allocated"))
                       (when (:bitmap-driven i) "bitmap")
                       (when-let [f (:mft-files i)]
                         (str f " files" (when-let [r (:mft-runs i)] (str ", " r " runs"))))
                       (when-let [x (:excluded-bytes i)] (str (log/human (long x)) " excluded"))]))))

(defn- cleanup-on-exit!
  "Registers f to run once, either when the command ends by itself or when the
   JVM is asked to stop — Ctrl+C, a system shutdown. Returns the thunk for the
   normal path.

   What it cannot do is survive TerminateProcess, and on Windows that is the
   normal way a detached run ends, not an exotic one: such a run owns no window,
   so taskkill refuses to end it without /F — and /F is TerminateProcess. This
   was measured on the machine, not assumed: taskkill without /F answered
   \"Die Beendigung dieses Prozesses muss erzwungen werden\" and the run carried
   on regardless.

   So the snapshot is not only released here. It is written down when it is
   taken, and the next run releases what a killed one left behind — see
   vss/release-stale!, or borg snapshot --stale to do it on demand."
  [f]
  (let [done (java.util.concurrent.atomic.AtomicBoolean. false)
        once (fn [] (when (.compareAndSet done false true)
                      (try (f) (catch Throwable _ nil))))
        hook (Thread. ^Runnable once "borg-imager-cleanup")]
    (.addShutdownHook (Runtime/getRuntime) hook)
    (fn []
      (once)
      (try (.removeShutdownHook (Runtime/getRuntime) hook) (catch Exception _ nil)))))

(defn- rate-window
  "Bytes per second over the last `span` seconds, in two readings.

   Against the wall clock, which is what the bar shows. And against each
   regime's own accumulated time, which is what the estimate needs: in a window
   spent almost entirely on reading, dividing the few zeroed bytes by wall time
   would report that the filling had become slow, when it had simply not been
   asked to do anything.

   A mean since the start can do neither. It hides an order-of-magnitude swing --
   and on a spinning disk the read rate swings exactly that much with how the
   data happens to lie."
  [^long span]
  (let [samples (volatile! [])]
    ;; no primitive hints here: Clojure carries at most four of them through a
    ;; fn, and the estimate needs five numbers.
    (fn [now read zeroed copy-ns zero-ns]
      (let [now (long now)
            cut (- now (* span 1000000000))
            v (vswap! samples
                      (fn [v] (conj (into [] (drop-while #(< (long (nth % 0)) cut) v))
                                    [now read zeroed copy-ns zero-ns])))
            a (nth v 0)
            b (peek v)
            d (fn [i] (- (long (nth b i)) (long (nth a i))))
            wall (/ (double (d 0)) 1e9)
            per (fn [bytes-at nanos-at]
                  (let [secs (/ (double (d nanos-at)) 1e9)]
                    (when (> secs 0.2) (/ (double (d bytes-at)) secs))))]
        (when (>= wall 0.5)
          {:wall-read (/ (double (d 1)) wall)
           :wall-fill (/ (double (d 2)) wall)
           :read (per 1 3)
           :fill (per 2 4)})))))

(defn- damped
  "An estimate may fall as fast as it likes and rises only slowly. A run that
   crosses into a fragmented stretch then gets the worse news in steps instead of
   all at once. Nothing is hidden -- the new value is reached, just not in one
   jump -- and the number keeps the shape that makes it worth reading at all."
  [prev now]
  (cond
    (nil? now) nil
    (nil? prev) now
    (< (double now) (double prev)) now
    :else (+ (double prev) (* 0.2 (- (double now) (double prev))))))

(defn- eta-seconds
  "What is left to read and what is left to merely zero-fill, each divided by the
   speed measured for that regime alone. One blended rate cannot do this: the two
   run orders of magnitude apart, so the estimate would swing by the same factor
   the moment the run crosses from one into the other. Nil while a regime that
   still has work has not been timed yet."
  [^long to-read ^long to-fill read-per-s fill-per-s]
  (let [part (fn [^long left per-s]
               (cond
                 (zero? left) 0.0
                 (and per-s (> (double per-s) 1.0)) (/ (double left) (double per-s))
                 :else nil))
        a (part to-read read-per-s)
        b (part to-fill fill-per-s)]
    (when (and a b) (+ (double a) (double b)))))

(defn- human-time ^String [secs]
  (if (or (nil? secs) (not (Double/isFinite (double secs))) (neg? (double secs)))
    "--"
    (let [s (long secs) h (quot s 3600) m (quot (mod s 3600) 60)]
      (if (pos? h)
        (log/fmt "%d:%02d:%02d" h m (mod s 60))
        (log/fmt "%d:%02d" m (mod s 60))))))

(defn- human-span
  "A range rather than a single number. Two estimates that differ say something
   a single one cannot: how much the recent minutes disagreed with each other.
   A span may narrow without ever contradicting itself, which is the whole point
   -- 30:00-50:00 that becomes 35:00-40:00 told no lie on the way."
  ^String [lo hi]
  (cond
    (nil? lo) (human-time nil)
    (or (nil? hi) (< (- (double hi) (double lo)) 1.0)) (human-time lo)
    :else (str (human-time lo) "-" (human-time hi))))

(defn- progress-reporter
  "Reports bytes, not blocks. The block grid restarts at every file run, so the
   blocks are nowhere near equal: counting them made the bar crawl through the
   dense front and race through the free tail — 9.9% of the blocks were 3.9% of
   the volume on the machine this was measured on."
  [^Path status quiet]
  (let [last (volatile! 0)
        tty (log/tty?)
        span 30
        window (rate-window span)
        ;; one entry per window length, not one per tick: eight samples taken a
        ;; second apart would be the same window seen eight times, and the
        ;; spread they describe would be no spread at all.
        history (volatile! [])
        marked (volatile! nil)
        eta-lo (volatile! nil)
        eta-hi (volatile! nil)
        base (volatile! nil)
        prev (volatile! nil)]
    (fn [{:keys [source block of stats length allocated]}]
      (let [size (long (:size stats 0))
            read (long (:read stats 0))
            zeroed (long (:zeroed stats 0))]
        ;; every source gets its own bar, but the counters run across all of
        ;; them. A new source starts from what the previous one left behind —
        ;; taking the baseline from the current sample would throw away every
        ;; block processed before the first report lands.
        (when (not= source (first @base))
          (vreset! base (if-let [p @prev]
                          [source (nth p 1) (nth p 2) (nth p 3)]
                          [source 0 0 0])))
        (vreset! prev [source size read zeroed])
        (let [now (System/nanoTime)]
          (when (or (>= (- now (long @last)) 1000000000) (= block (dec (long of))))
            (vreset! last now)
            (let [[_ b-size b-read b-zero] @base
                  covered (- size (long b-size))
                  read* (- read (long b-read))
                  zeroed* (- zeroed (long b-zero))
                  length (long (or length 0))
                  allocated (long (or allocated 0))
                  to-read (max 0 (- allocated read*))
                  to-fill (max 0 (- (- length allocated) zeroed*))
                  pct (if (pos? length) (* 100.0 (/ (double covered) (double length))) 0.0)
                  copy-ns (long (:copy-nanos stats 0))
                  zero-ns (long (:zero-nanos stats 0))
                  w (window now read zeroed copy-ns zero-ns)
                  rr (:wall-read w)
                  fr (:wall-fill w)
                  copy-s (/ (double copy-ns) 1e9)
                  zero-s (/ (double zero-ns) 1e9)
                  read-speed (when (pos? copy-s) (/ (double read) copy-s))
                  fill-speed (when (pos? zero-s) (/ (double zeroed) zero-s))
                  ;; the recent window is what the estimate goes by; the mean
                  ;; since the start only stands in until a window exists.
                  read-now (or (:read w) read-speed)
                  fill-now (or (:fill w) fill-speed)
                  ;; the very first reading is the least trustworthy one there
                  ;; is -- it covers the connect and the first block. Kept, it
                  ;; would anchor the span to itself for the next four minutes,
                  ;; so the clock starts at it rather than with it.
                  _ (when read-now
                      (if (nil? @marked)
                        (vreset! marked now)
                        (when (> (- now (long @marked)) (* span 1000000000))
                          (vreset! marked now)
                          (vswap! history (fn [h] (vec (take-last 8 (conj h [read-now fill-now]))))))))
                  rates (conj @history [read-now fill-now])
                  pick (fn [f xs cur] (if (seq xs) (apply f xs) cur))
                  reads (keep first rates)
                  fills (keep second rates)
                  lo (eta-seconds to-read to-fill
                                  (pick max reads read-now) (pick max fills fill-now))
                  hi (eta-seconds to-read to-fill
                                  (pick min reads read-now) (pick min fills fill-now))
                  lo (vreset! eta-lo (damped @eta-lo lo))
                  hi (vreset! eta-hi (damped @eta-hi hi))
                  eta (when (and lo hi) (/ (+ (double lo) (double hi)) 2.0))]
              (status! status {:phase :copy :source source
                               :block (inc (long block)) :of of
                               :percent pct :covered covered :length length
                               :read read* :to-read to-read
                               :zeroed zeroed* :to-fill to-fill
                               :sent (long (:sent stats 0))
                               :read-per-second (when rr (long rr))
                               :fill-per-second (when fr (long fr))
                               :read-speed (when read-speed (long read-speed))
                               :fill-speed (when fill-speed (long fill-speed))
                               :eta-seconds (when eta (long eta))
                               :eta-low (when lo (long lo))
                               :eta-high (when hi (long hi))})
              (when-not quiet
                (binding [*out* *err*]
                  (print (str (when tty "\r")
                              (format "%-10s %5.1f%%  %s of %s  read %s/s  fill %s/s  new %s  eta %s   "
                                      source pct (log/human covered) (log/human length)
                                      (if rr (log/human (long rr)) "--")
                                      (if fr (log/human (long fr)) "--")
                                      (log/human (long (:sent stats 0)))
                                      (human-span lo hi))
                              (when-not tty "\n")))
                  (flush))))))))))

(defn cmd-image [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        specs (mapv parse-source (rest args))
        raw (contains? opts "--raw")
        align (not= "off" (get opts "--align"))
        excludes (cond-> (if (contains? opts "--no-default-excludes") [] wv/default-excludes)
                   (get opts "--exclude")
                   (into (remove str/blank? (str/split (get opts "--exclude") #"[,;]"))))
        min-run (parse-size (or (get opts "--min-run") "256K"))
        block (check-block (parse-size (or (get opts "--block") "4M")))
        mode (keyword (or (get opts "--snapshot") "auto"))
        status (when-let [f (get opts "--status")] (path f))
        stop-file (when-let [f (get opts "--stop")] (path f))]
    ;; eine schon liegende Stoppdatei kann nicht diesen Lauf gemeint haben
    (when (and stop-file (Files/exists stop-file (into-array java.nio.file.LinkOption [])))
      (Files/deleteIfExists stop-file)
      (log/line (str "NOTE      a stop request was already lying at " stop-file
                     " -- removed, it cannot have meant this run")))
    (log/line (format "start     image %s::%s from %s  block %s  compression %s  pid %d"
                      repo archive (str/join " " (rest args)) (log/human block)
                      (or (get opts "--compression") "zstd,3")
                      (.pid (java.lang.ProcessHandle/current))))
    (status! status {:phase :start :archive archive :repo repo})
    (when (empty? specs) (throw (ex-info "usage: borgi image REPO::ARCHIVE SOURCE..." {})))
    (when-not (zero? (mod block 512))
      (throw (ex-info "--block must be a multiple of 512" {:block block})))
    (let [specs (expand-sources specs)
          want (when (not= :none mode) (volumes-to-snapshot specs))
          t-stale (System/nanoTime)
          left-over (when (seq want) (vss/release-stale!))
          _ (when (seq left-over)
              (log/step "stale" t-stale
                        (format "released %s left behind by a run that was killed"
                                (str/join " " (mapcat :ids left-over)))))
          t-snap (System/nanoTime)
          snapshot (when (seq want)
                     (or (vss/take-snapshot! want (when (not= :auto mode) {:provider mode}))
                         (throw (ex-info (str "no VSS provider could snapshot " (str/join " " want)
                                              " -- install vshadow.exe, or pass --snapshot none to read the live volume")
                                         {:volumes want}))))]
      (when snapshot
        (vss/remember! snapshot want)
        (log/step "snapshot" t-snap
                  (format "%s: %s%s" (name (:provider snapshot))
                          (str/join " " (keys (:devices snapshot)))
                          (if (:atomic snapshot) " (one consistent set)" " (per volume, not atomic)")))
        (status! status {:phase :snapshot :provider (:provider snapshot)}))
      (when (and (seq want) (not snapshot))
        (log/line "WARNING   no snapshot -- the image is crash-consistent at best"))
      (when (and status (seq want))
        (let [root (str/upper-case (str (.getRoot (.toAbsolutePath status))))]
          (when (some #(str/starts-with? root (str/upper-case (str % ":"))) want)
            (log/line (str "WARNING   the status file is on " root
                           " -- the volume being imaged. Every write forces copy-on-write"
                           " into the shadow copy and seeks against the read stream;"
                           " put it on another volume.")))))
      (let [t-src (System/nanoTime)
            _ (status! status {:phase :scanning})
            sources (open-sources specs snapshot
                                  {:raw raw :align align :excludes excludes :min-run min-run
                                   :progress (scan-reporter status (contains? opts "--quiet"))})
            _ (doseq [s sources] (log/step "source" t-src (source-detail s)))
            conn* (volatile! nil)
            release! (cleanup-on-exit!
                      (fn []
                        (when-let [c @conn*] (try (remote/close c) (catch Exception _ nil)))
                        (doseq [s sources] (try (src/s-close s) (catch Exception _ nil)))
                        (when snapshot
                          (try ((:release! snapshot)) (catch Exception _ nil))
                          (try (vss/forget! (:ids snapshot)) (catch Exception _ nil)))))]
        (try
          (when (contains? opts "--dry-run")
            (doseq [s sources]
              (let [p (image/plan s block)
                    i (src/s-info s)]
                (println (format "%s  %s" (src/s-name s) (image/image-name s)))
                (println (format "  %-22s %s" "length" (log/human (long (src/s-length s)))))
                (println (format "  %-22s %s" "allocated" (log/human (long (:allocated p)))))
                (println (format "  %-22s %s" "chunking" (name (or (:chunking i) :fixed))))
                (println (format "  %-22s %d of %s" "blocks" (:count p) (log/human block)))
                (println (format "  %-22s %d" "cuts" (:cuts p)))
                (doseq [[k lbl] [[:mft-files "mft files"] [:mft-runs "mft runs"]
                                 [:cluster-size "cluster size"] [:total-clusters "clusters"]
                                 [:free-clusters "free clusters"] [:bitmap-driven "bitmap driven"]
                                 [:ntfs "ntfs"] [:sector-size "sector size"]]
                        :when (some? (get i k))]
                  (println (format "  %-22s %s" lbl (get i k))))
                (when-let [ex (:excluded-bytes i)]
                  (println (format "  %-22s %s" "excluded" (log/human (long ex)))))
                (when (= :disk (:kind i))
                  (println (format "  %-22s %s %d partitions, snapshotted %s"
                                   "disk" (name (:style i)) (count (:partitions i))
                                   (str/join "," (:snapshotted i))))))))
          (when-not (contains? opts "--dry-run")
            (let [t-conn (System/nanoTime)
                  [conn rpath] (store/connect repo opts)]
              (vreset! conn* conn)
              (log/step "connect" t-conn rpath)
              (status! status {:phase :connected})
              (let [t-open (System/nanoTime)
                    repo-id (b/hex (remote/open! conn rpath :create (contains? opts "--init")))
                    k (key/plaintext-key (or (get opts "--compression") "zstd,3"))
                    _ (log/step "repo" t-open (str "opened, id " (subs repo-id 0 12) "..."))
                    t-init (System/nanoTime)
                    _ (when (and (contains? opts "--init") (create/init-repo! conn k))
                        (log/step "init" t-init "empty manifest committed"))
                    res (create/create-image!
                         conn k
                         archive sources
                         {:repo-id (when-not (contains? opts "--no-cache") repo-id)
                          :block-size block
                          :progress (let [quiet (contains? opts "--quiet")]
                                      (when (or status log/*progress* (not quiet))
                                        (progress-reporter status quiet)))
                          :stop? (stop-watch stop-file)
                          :extra {:snapshot (when snapshot
                                              {:provider (:provider snapshot)
                                               :atomic (:atomic snapshot)
                                               :devices (:devices snapshot)})}})]
                (binding [*out* *err*] (println))
                (println (format "archive %s: %s image, %s read from disk, %s free space zeroed"
                                 (:archive res) (log/human (long (:size res)))
                                 (log/human (long (:read res))) (log/human (long (:zeroed res)))))
                (println (format "  %d blocks of %s, %d new chunks (%s sent), %d chunks in repo"
                                 (:blocks res) (log/human block) (:new-chunks res)
                                 (log/human (long (:sent res))) (:chunks res)))
                (status! status {:phase :done :archive (:archive res) :size (:size res)
                                 :read (:read res) :sent (:sent res)}))))
          (catch Throwable t
            (if (:stopped (ex-data t))
              (let [d (ex-data t)]
                (when stop-file (try (Files/deleteIfExists stop-file) (catch Exception _ nil)))
                (log/line (format "stopped   on request at block %d of %d -- nothing was committed"
                                  (inc (long (:block d 0))) (long (:of d 0))))
                (status! status (merge {:phase :stopped} (dissoc d :stopped))))
              (do
                (log/line "FAILED   " (str (.getName (class t)) ": " (or (ex-message t) "")))
                (status! status {:phase :failed
                                 :error (or (ex-message t) "")
                                 :error-class (.getName (class t))})))
            (throw t))
          (finally (release!)))))))

(defn- disk-source-of [info]
  (or (first (filter #(= :disk (:kind %)) (:sources info)))
      (throw (ex-info "this archive holds no whole-disk image -- layout needs one" {}))))

(defn shrink-floor
  "Vorabschaetzung, wie weit NTFS schrumpfen kann. Nicht der hoechste belegte
   Cluster -- ntfsresize raeumt nach vorn; es sind die belegten Bytes plus Luft.
   Die genaue Zahl nennt `ntfsresize --info`."
  [v ^long length]
  (when-let [a (:allocated-bytes v)]
    (min length (+ (long a) (max (* 64 1024 1024) (quot (long a) 10))))))

(defn- shrink-plan
  "Partitionen auf target umgerechnet; die groesste nimmt das ganze Defizit auf."
  [ds ^long target ^long sector force]
  (let [parts (vec (sort-by :offset (:partitions ds)))
        orig (long (:length ds))
        deficit (- orig target)
        vol-of (fn [p] (first (filter #(= (long (:offset %)) (long (:offset p))) (:volumes ds))))]
    (if (<= deficit 0)
      parts
      (let [victim (apply max-key #(long (:length %)) parts)
            floor (shrink-floor (vol-of victim) (long (:length victim)))
            new-len (- (long (:length victim)) deficit)]
        (when (<= new-len 0)
          (throw (ex-info "target disk is too small for this layout" {:deficit deficit})))
        (when (and floor (< new-len (long floor)) (not force))
          (throw (ex-info (format "partition %d would shrink to %s but needs at least %s -- pass --force to try anyway"
                                  (:number victim) (log/human new-len) (log/human (long floor)))
                          {:partition (:number victim)})))
        (loop [[p & more] parts shift 0 out []]
          (if-not p
            out
            (let [hit (= (:number p) (:number victim))
                  np (cond-> (update p :offset - shift)
                       hit (assoc :length new-len))]
              (recur more (if hit (+ shift deficit) shift) (conj out np)))))))))

(defn cmd-restore-image [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)]
    (when-not archive (throw (ex-info "usage: borgi restore-image REPO::ARCHIVE --to TARGET" {})))
    (try
      (let [items (arch/items r k (arch/archive r k m archive))
            item (restore/pick-image items (get opts "--image"))
            pn (some-> (get opts "--partition") parse-long)
            ;; Read the partition table only when a partition is asked for:
            ;; otherwise every whole-disk restore would cost one more chunk
            ;; fetch, for nothing.
            im (cond-> (restore/manifest-of r k items)
                 pn (restore/with-table (b/display (:path item)) (restore/image-reader r k item)))
            rng (when-let [x (get opts "--range")]
                  (let [[a b] (str/split x #":" 2)]
                    (when-not b (throw (ex-info "--range wants FROM:LENGTH, e.g. 0:440" {})))
                    [(parse-size a) (parse-size b)]))
            part (when pn
                   (let [ds (disk-source-of im)]
                     (or (first (filter #(= pn (:number %)) (:partitions ds)))
                         (throw (ex-info (str "no partition " pn " in this image")
                                         {:have (mapv :number (:partitions ds))})))))
            total (cond part (long (:length part))
                        rng (long (second rng))
                        :else (arch/item-size item))
            to (or (get opts "--to") (throw (ex-info "restore-image needs --to TARGET" {})))
            ;; Auf Linux gibt es keine Geraetenamen im Windows-Sinn. /dev/sda
            ;; ist dort eine Datei, und der Dateiweg schreibt richtig hinein:
            ;; FileChannel oeffnet ohne Truncate und meldet die echte
            ;; Geraetegroesse. --to-file bleibt fuer den Fall, dass wirklich
            ;; eine Abbilddatei gemeint ist.
            target (if (or (contains? opts "--to-file") (linux?))
                     {:kind :file :path to}
                     (wv/parse-target to))
            vhd (contains? opts "--vhd")
            sink (if (= :file (:kind target))
                   (restore/file-sink (path (:path target)))
                   (wv/open-device-sink target {:force (contains? opts "--force")}))]
        ;; Die Pruefung haengt daran, ob das Ziel wachsen kann, nicht an seiner
        ;; Art: eine gewoehnliche Datei wird groesser, ein Blockgeraet nicht.
        ;; Frueher entschied :file darueber -- damit waere unter Linux jede
        ;; Groessenpruefung entfallen, weil dort jedes Ziel :file ist.
        ;; Die Groesse VOR dem Schliessen merken: danach wirft sink-size eine
        ;; ClosedChannelException, und die verdeckte bisher die eigentliche
        ;; Meldung.
        (let [have (long (restore/sink-size sink))]
          (when (and (not (:regular? sink)) (< have total))
            (restore/sink-close sink)
            (throw (ex-info (format "target holds %s but the image is %s"
                                    (log/human have) (log/human total)) {}))))
        (try
          (let [res (restore/write-image!
                     r k item sink
                     {:skip-zeros (contains? opts "--skip-zeros")
                      :from (cond part (:offset part) rng (first rng))
                      :span (cond part (:length part) rng (second rng))
                      :progress (let [t0 (System/nanoTime) last (volatile! 0)]
                                  (fn [{:keys [done of]}]
                                    (let [now (System/nanoTime)]
                                      (when (>= (- now @last) 1000000000)
                                        (vreset! last now)
                                        (let [rate (long (/ (double done)
                                                            (max 0.001 (/ (- now t0) 1e9))))
                                              pct (* 100.0 (/ (double done) (max 1.0 (double of))))]
                                          (status! nil {:phase :restore :percent pct
                                                        :done done :of of :per-second rate})
                                          (binding [*out* *err*]
                                            (print (format "\r%5.1f%%  %s of %s  %s/s   "
                                                           pct (log/human (long done))
                                                           (log/human (long of)) (log/human rate)))
                                            (flush)))))))})]
            (restore/set-size! sink (:length res))
            ;; the throttled ticks can stop short of the end; a finished restore
            ;; is finished, and the interface draws what it last heard
            (status! nil {:phase :restore :percent 100.0
                          :done (:length res) :of (:length res)})
            (when vhd
              (restore/w! sink (:length res) (restore/vhd-footer (long (:length res))) 512))
            (binding [*out* *err*] (println))
            (println (format "restored %s%s (%s written, %s skipped as zero) to %s"
                             (b/display (:path item))
                             (cond part (str " partition " pn)
                                   rng (format " bytes %d+%d" (long (first rng)) (long (second rng)))
                                   :else "")
                             (log/human (long (:written res)))
                             (log/human (long (:skipped res))) to))
            (when (and im (= :file (:kind target)) (not vhd))
              (println "  raw image -- mount it with `Mount-DiskImage` after `borgi restore-image --vhd`")))
          (finally (restore/sink-close sink))))
      (finally (store/close r)))))

(defn layout-script
  "sfdisk-Skript der Partitionstabelle; Disk-Id und GUIDs bleiben, damit die BCD
   weiter aufloest."
  ^String [ds ^long target force]
  (let [sector (long (or (:sector-size ds) 512))
        parts (shrink-plan ds target sector force)
        sec (fn [^long b] (quot b sector))
        gpt (= :gpt (:style ds))
        lines (concat
               [(if gpt "label: gpt" "label: dos")]
               (when-let [id (:disk-id ds)] [(str "label-id: " id)])
               (when-let [sig (:mbr-signature ds)] [(format "label-id: 0x%08x" (long sig))])
               ["unit: sectors"]
               (when gpt ["first-lba: 34"])
               [""]
               (for [p parts]
                 (str/join ", "
                           (cond-> [(format "start=%d" (sec (long (:offset p))))
                                    (format "size=%d" (sec (long (:length p))))]
                             gpt (into (cond-> [(str "type=" (:type-guid p))
                                                (str "uuid=" (:part-guid p))]
                                         (seq (:label p)) (conj (format "name=\"%s\"" (:label p)))))
                             (not gpt) (into (cond-> [(format "type=%02x" (long (or (:mbr-type p) 7)))]
                                               (:boot p) (conj "bootable")))))))]
    (str (str/join "\n" lines) "\n")))

(defn cmd-layout [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)]
    (try
      (let [items (arch/items r k (arch/archive r k m archive))
            info (or (restore/manifest-of r k items)
                     (throw (ex-info "no imager.edn in this archive" {})))
            ds (disk-source-of info)
            target (long (if-let [t (get opts "--disk-size")] (parse-size t) (:length ds)))]
        (binding [*out* *err*]
          (println (format "source disk %s, target %s%s"
                           (log/human (long (:length ds))) (log/human target)
                           (if (< target (long (:length ds))) " (shrinking)" ""))))
        (print (layout-script ds target (contains? opts "--force")))
        (flush))
      (finally (store/close r)))))

(defn- kv [k v] (when (some? v) (println (str (name k) "=" v))))

(defn cmd-plan [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)]
    (try
      (let [items (arch/items r k (arch/archive r k m archive))
            item (restore/pick-image items (get opts "--image"))
            info (or (restore/manifest-of r k items)
                     (throw (ex-info "no imager.edn in this archive" {})))
            nm (b/display (:path item))
            srcs (:sources info)
            ds (or (first (filter #(= nm (:image %)) srcs)) (first srcs))
            pn (some-> (get opts "--partition") parse-long)
            part (when pn (or (first (filter #(= pn (:number %)) (:partitions ds)))
                              (throw (ex-info (str "no partition " pn " in this image")
                                              {:have (mapv :number (:partitions ds))}))))
            vol (when part (first (filter #(= (long (:offset part)) (long (:offset %))) (:volumes ds))))
            high (:highest-allocated-byte (or vol ds))]
        (kv :image nm)
        (kv :kind (name (:kind ds)))
        (kv :block_size (:block-size info))
        (kv :sector_size (:sector-size ds))
        (kv :archive_bytes (arch/item-size item))
        (if part
          (do (kv :partition pn)
              (kv :offset (:offset part))
              (kv :length (:length part))
              (kv :letter (:letter vol))
              (kv :allocated (:allocated-bytes vol))
              (kv :highest_allocated high)
              (kv :min_shrink (shrink-floor vol (long (:length part)))))
          (do (kv :length (:length ds))
              (kv :letter (:letter ds))
              (kv :allocated (:allocated-bytes ds))
              (kv :bitmap (:bitmap-driven ds))
              (kv :ntfs (:ntfs ds))
              (kv :cluster_size (:cluster-size ds))
              (kv :highest_allocated high)
              (kv :min_shrink (shrink-floor ds (long (:length ds))))))
        (when (and (not part) (= :disk (:kind ds)))
          (kv :style (name (:style ds)))
          (kv :partitions (count (:partitions ds)))
          (doseq [p (sort-by :number (:partitions ds))]
            (let [v (first (filter #(= (long (:offset p)) (long (:offset %))) (:volumes ds)))
                  n (:number p)]
              (kv (keyword (str "p" n ".offset")) (:offset p))
              (kv (keyword (str "p" n ".length")) (:length p))
              (kv (keyword (str "p" n ".type")) (or (:type-guid p) (:mbr-type p)))
              (kv (keyword (str "p" n ".label")) (:label p))
              (kv (keyword (str "p" n ".letter")) (:letter v))
              (kv (keyword (str "p" n ".min_shrink")) (shrink-floor v (long (:length p))))))))
      (finally (store/close r)))))

(defn- where-line ^String [{:keys [image letter partition]}]
  (format "%s%s%s" image
          (if partition (format ", partition %d" partition) "")
          (if letter (format " (%s:)" letter) "")))

(defn cmd-ls [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)]
    (try
      (let [items (arch/items r k (arch/archive r k m archive))
            where (restore/open-image-volume r k items (get opts "--image") (some-> (get opts "--partition") parse-long))
            vol (:vol where)
            path (or (second args) "/")
            hit (or (ntfs/lookup vol path)
                    (throw (ex-info (str "not in this image: " path) {})))]
        (binding [*out* *err*] (println (where-line where)))
        (if-not (:dir? hit)
          (let [i (ntfs/file-info vol (:frn hit))]
            (println (format "%12s  %s" (log/human (long (:size i))) path))
            (when-not (:readable? i) (println (str "             " (:why i)))))
          (doseq [e (ntfs/dir-entries vol (long (:frn hit)))]
            (println (format "%12s  %s%s"
                             (if (:dir? e) "" (log/human (long (:size e))))
                             (:name e)
                             (if (:dir? e) "/" ""))))))
      (finally (store/close r)))))

(defn- get-file!
  "Bytezahl, oder nil samt Grund auf stderr. Bricht keinen Baumdurchgang ab."
  [vol e ^Path out quiet]
  (let [i (ntfs/file-info vol (long (:frn e)))]
    (if-not (:readable? i)
      (do (binding [*out* *err*] (println (format "  uebersprungen  %s -- %s" (str out) (:why i))))
          nil)
      (do (when-let [d (.getParent out)]
            (Files/createDirectories d (into-array java.nio.file.attribute.FileAttribute [])))
          (with-open [os (Files/newOutputStream
                          out (into-array OpenOption [StandardOpenOption/CREATE
                                                      StandardOpenOption/TRUNCATE_EXISTING
                                                      StandardOpenOption/WRITE]))]
            (let [n (ntfs/read-file! vol (long (:frn e)) os)]
              (when-not quiet
                (println (format "%12s  %s" (log/human (long n)) (str out))))
              n))))))

(defn- get-tree!
  "Eintrag samt allem darunter. seen bricht einen Index, der im Kreis zeigt."
  [vol e ^Path out quiet seen]
  (let [frn (long (:frn e))]
    (cond
      (not (:dir? e))
      (let [n (get-file! vol e out quiet)]
        {:files (if n 1 0) :bytes (or n 0) :skipped (if n 0 1)})

      (contains? seen frn) {:files 0 :bytes 0 :skipped 0}

      :else
      (do (Files/createDirectories out (into-array java.nio.file.attribute.FileAttribute []))
          (reduce (fn [acc c]
                    (merge-with + acc (get-tree! vol c (.resolve out ^String (:name c))
                                                 quiet (conj seen frn))))
                  {:files 0 :bytes 0 :skipped 0}
                  (ntfs/dir-entries vol frn))))))

(defn cmd-get [{:keys [args opts]}]
  (let [{:keys [repo archive]} (split-location (first args))
        [r k m] (open-store repo opts)]
    (try
      (let [items (arch/items r k (arch/archive r k m archive))
            where (restore/open-image-volume r k items (get opts "--image") (some-> (get opts "--partition") parse-long))
            vol (:vol where)
            src (second args)
            hit (or (ntfs/lookup vol src)
                    (throw (ex-info (str "not in this image: " src) {})))
            base (or (last (remove str/blank? (str/split src #"[\\/]+"))) archive)
            ;; --to wie bei cp -- sonst legt `--to /mnt` eine DATEI namens /mnt an
            to (let [t (path (or (get opts "--to") base))]
                 (if (Files/isDirectory t (into-array java.nio.file.LinkOption []))
                   (.resolve t ^String base)
                   t))
            quiet (contains? opts "--quiet")]
        (binding [*out* *err*] (println (where-line where)))
        (let [res (get-tree! vol hit to quiet #{})]
          (println (format "%d files, %s written to %s%s"
                           (:files res) (log/human (long (:bytes res))) (str to)
                           (if (pos? (long (:skipped res)))
                             (format ", %d skipped" (:skipped res)) "")))))
      (finally (store/close r)))))

(defn cmd-disks [_]
  (let [vols (wv/volumes)
        disks (wv/disks)]
    (println "disks")
    (doseq [d disks]
      (println (format "  disk:%-3d %10s  %-4s %d partitions  sector %d"
                       (:disk d) (log/human (long (:length d))) (name (:style d))
                       (count (:partitions d)) (:sector-size d)))
      (doseq [p (:partitions d)]
        (println (format "    #%-2d %10s at %-12s %s"
                         (:number p) (log/human (long (:length p))) (:offset p)
                         (or (:label p) (some-> (:mbr-type p) (->> (format "type 0x%02x"))) "")))))
    (println "volumes")
    (doseq [v vols]
      (if (:error v)
        (println (format "  %-3s %s" (str (:letter v) ":") (:error v)))
        (println (format "  %-3s %10s  %s  clusters %s used of %s  disk %s"
                         (str (:letter v) ":") (log/human (long (or (:length v) 0)))
                         (if (:ntfs v) "ntfs" "other")
                         (if (:total-clusters v)
                           (- (long (:total-clusters v)) (long (or (:free-clusters v) 0)))
                           "?")
                         (or (:total-clusters v) "?")
                         (str/join "," (map :disk (:extents v)))))))
    (println "vss providers")
    (doseq [p (vss/probe)]
      (println (format "  %-10s %s" (name (:provider p)) (if (:available p) "available" "-"))))))

(defn- cmd-snapshot* [args opts]
  (let [vols (if (seq args) (vec args) [(or (System/getenv "SystemDrive") "C:")])
        mode (keyword (or (get opts "--snapshot") "auto"))
        s (vss/take-snapshot! vols (when (not= :auto mode) {:provider mode}))]
    (if-not s
      (throw (ex-info (str "no VSS provider could snapshot " (str/join " " vols)) {}))
      (try
        (println (format "provider %s%s" (name (:provider s)) (if (:atomic s) " (atomic set)" "")))
        (doseq [[l d] (:devices s)] (println (format "  %-3s %s" l d)))
        (finally
          (when-not (contains? opts "--keep")
            ((:release! s))
            (println "released")))))))

(defn cmd-snapshot [{:keys [args opts]}]
  (if (contains? opts "--stale")
    (let [dead (vss/release-stale!)]
      (if (seq dead)
        (doseq [r dead]
          (println (format "released %s (%s, taken %s by pid %s, which is gone)"
                           (str/join " " (:ids r)) (name (or (:provider r) :unknown))
                           (:at r) (:pid r))))
        (println "nothing to release")))
    (cmd-snapshot* args opts)))

(declare run)

(defn cmd-ui [{:keys [opts]}]
  (ui/serve! {:port (parse-long (or (get opts "--port") "8722"))
              :no-browser (contains? opts "--no-browser")
              :url-file (get opts "--url-file")
              :run run})
  ;; the interface is the whole run: everything happens in the threads it started
  @(promise))

(defn cmd-gui [{:keys [opts]}]
  (gui/show! {:fullscreen (contains? opts "--fullscreen") :run run})
  ;; das Fenster ist der ganze Lauf; Schliessen beendet das Programm
  @(promise))

(def verbs
  [{:verb "disks" :args [] :doc "list disks, volumes and VSS providers" :fn cmd-disks}
   {:verb "image" :args ["REPO::ARCHIVE" "SOURCE..."]
    :opts [["--snapshot" "MODE" "auto (default), none, vshadow, wmi or vssadmin"]
           ["--block" "SIZE" "fixed chunk size, default 4M"]
           ["--compression" "SPEC" "none, lz4, zstd or zstd,LEVEL -- default zstd,3"]
           ["--align" "MODE" "files (default) restarts the chunk grid at each file run; off for a plain grid"]
           ["--min-run" "SIZE" "smallest file run that gets its own chunk boundary, default 256K"]
           ["--exclude" "PATHS" "extra volume-relative paths to store as zeros, comma separated"]
           ["--no-default-excludes" "" "keep pagefile.sys, hiberfil.sys, swapfile.sys and the recycle bin"]
           ["--raw" "" "read every sector, ignore the allocation bitmap"]
           ["--quiet" "" "no progress on stderr"]
           ["--status" "FILE" "also write the progress to FILE as one line of EDN, so a detached run can be watched"]
           ["--stop" "FILE" "stop cleanly once FILE appears -- the only way to end a detached run without losing the snapshot"]
           ["--dry-run" "" "open the sources, print what would be read, touch no repository"]
           ["--server-cmd" "CMD" "spawn CMD instead of ssh (for a local borg serve)"]
           ["--rsh" "CMD" "spawn an external ssh binary instead of the built-in client"]
           ["--remote-path" "PATH" "path to borg on the server (default borg, or $BORG_REMOTE_PATH)"]
           ["--identity" "KEY" "ssh private key (default ~/.ssh/id_*, or $BORG_SSH_KEY)"]
           ["--user" "NAME" "ssh user if the location does not name one"]
           ["--init" "" "create the repository if it does not exist"]
           ["--no-cache" "" "ignore the local chunk cache (re-lists the repository)"]]
    :doc "image volumes or disks into a borg repository" :fn cmd-image}
   {:verb "restore-image" :args ["REPO::ARCHIVE"]
    :opts [["--to" "TARGET" "D:, disk:1, \\\\.\\PhysicalDrive1 or a file path"]
           ["--to-file" "" "treat --to as a plain file even if it looks like a device"]
           ["--image" "NAME" "which image in the archive (default: the only one)"]
           ["--partition" "N" "restore only partition N of a whole-disk image, at offset 0"]
           ["--range" "FROM:LEN" "restore only this byte range, at offset 0 (e.g. 0:440 for MBR code)"]
           ["--vhd" "" "append a fixed-VHD footer so Windows can mount the file"]
           ["--skip-zeros" "" "do not write all-zero blocks (fresh or trimmed targets)"]
           ["--force" "" "allow writing the disk that carries the running system"]
           ["--server-cmd" "CMD" "spawn CMD instead of ssh"]
           ["--rsh" "CMD" "spawn an external ssh binary"]
           ["--remote-path" "PATH" "path to borg on the server"]
           ["--identity" "KEY" "ssh private key"]
           ["--user" "NAME" "ssh user"]]
    :doc "write an image back to a disk, a volume or a file" :fn cmd-restore-image}
   {:verb "plan" :args ["REPO::ARCHIVE"]
    :opts [["--image" "NAME" "which image in the archive"]
           ["--partition" "N" "report one partition of a whole-disk image"]
           ["--server-cmd" "CMD" "spawn CMD instead of ssh"]
           ["--rsh" "CMD" "spawn an external ssh binary"]
           ["--remote-path" "PATH" "path to borg on the server"]
           ["--identity" "KEY" "ssh private key"]
           ["--user" "NAME" "ssh user"]]
    :doc "print key=value facts about an image, for scripts" :fn cmd-plan}
   {:verb "ls" :args ["REPO::ARCHIVE" "PATH..."]
    :opts [["--image" "NAME" "which image in the archive (default: the only one)"]
           ["--partition" "N" "which partition of a whole-disk image (default: the first NTFS one)"]
           ["--server-cmd" "CMD" "spawn CMD instead of ssh"]
           ["--rsh" "CMD" "spawn an external ssh binary"]
           ["--remote-path" "PATH" "path to borg on the server"]
           ["--identity" "KEY" "ssh private key"]
           ["--user" "NAME" "ssh user"]]
    :doc "list a directory inside an image, without unpacking it" :fn cmd-ls}
   {:verb "get" :args ["REPO::ARCHIVE" "PATH"]
    :opts [["--to" "DEST" "where to write it (default: the name it has in the image)"]
           ["--image" "NAME" "which image in the archive (default: the only one)"]
           ["--partition" "N" "which partition of a whole-disk image (default: the first NTFS one)"]
           ["--quiet" "" "do not name every file"]
           ["--server-cmd" "CMD" "spawn CMD instead of ssh"]
           ["--rsh" "CMD" "spawn an external ssh binary"]
           ["--remote-path" "PATH" "path to borg on the server"]
           ["--identity" "KEY" "ssh private key"]
           ["--user" "NAME" "ssh user"]]
    :doc "fetch one file or one directory tree out of an image" :fn cmd-get}
   {:verb "layout" :args ["REPO::ARCHIVE"]
    :opts [["--disk-size" "SIZE" "target disk size; the largest partition absorbs the difference"]
           ["--force" "" "allow shrinking a partition below its highest allocated byte"]
           ["--server-cmd" "CMD" "spawn CMD instead of ssh"]
           ["--rsh" "CMD" "spawn an external ssh binary"]
           ["--remote-path" "PATH" "path to borg on the server"]
           ["--identity" "KEY" "ssh private key"]
           ["--user" "NAME" "ssh user"]]
    :doc "print an sfdisk script that recreates the disk's partition table" :fn cmd-layout}
   {:verb "snapshot" :args []
    :opts [["--snapshot" "MODE" "force one provider"]
           ["--keep" "" "do not release the snapshot again"]
           ["--stale" "" "release snapshots a killed run left behind, take none"]]
    :doc "take a VSS snapshot and print its device path" :fn cmd-snapshot}
   {:verb "list" :args ["REPO[::ARCHIVE]"] :doc "list archives, or files in an archive" :fn cmd-list}
   {:verb "info" :args ["REPO[::ARCHIVE]"] :doc "show repository / archive / image info" :fn cmd-info}
   {:verb "extract" :args ["REPO::ARCHIVE"] :opts [["--dest" "DIR" "target directory (default .)"]]
    :doc "extract an archive as files" :fn cmd-extract}
   {:verb "verify" :args ["REPO"] :doc "check every object: segment crc + chunk id" :fn cmd-verify}
   {:verb "chunk" :args ["FILE"] :doc "print borg chunk ids of a file" :fn cmd-chunk}
   {:verb "create" :args ["REPO::ARCHIVE" "PATH..."]
    :opts [["--server-cmd" "CMD" "spawn CMD instead of ssh (for a local borg serve)"]
           ["--rsh" "CMD" "spawn an external ssh binary instead of the built-in client"]
           ["--remote-path" "PATH" "path to borg on the server (default borg, or $BORG_REMOTE_PATH)"]
           ["--identity" "KEY" "ssh private key (default ~/.ssh/id_*, or $BORG_SSH_KEY)"]
           ["--user" "NAME" "ssh user if the location does not name one"]
           ["--compression" "SPEC" "none (default), lz4, zstd or zstd,LEVEL"]
           ["--init" "" "create the repository if it does not exist"]
           ["--no-cache" "" "ignore the local files/chunks cache"]]
    :doc "back up files to a borg serve target" :fn cmd-create}
   {:verb "ui" :args []
    :opts [["--port" "N" "first port to try, default 8722"]
           ["--no-browser" "" "only print the link, open nothing"]
           ["--url-file" "PATH" "also write the link here, for a kiosk script in the recovery system"]]
    :doc "serve the local web interface on 127.0.0.1 and open it" :fn cmd-ui}
   {:verb "gui" :args []
    :opts [["--fullscreen" "" "fill the screen without a frame, for the recovery system"]]
    :doc "open the interface as a window" :fn cmd-gui}
   {:verb "platform" :args [] :doc "report host capabilities of the platform seam" :fn cmd-platform}])

(def by-verb (into {} (map (juxt :verb identity) verbs)))

(defn usage []
  (println "borg-imager -- a Windows imager that writes into a borg repository")
  (println)
  (println "  borgi image ssh://user@host/srv/repo::$(hostname)-$(date +%F) C: --init")
  (println)
  (doseq [{:keys [verb args opts doc]} verbs]
    (println (format "  %-14s %-24s %s" verb (str/join " " args) doc))
    (doseq [[o a d] opts]
      (println (format "  %-14s   %-22s %s" "" (str o (when (seq a) (str " " a))) d)))))

(defn- flags-of [spec]
  (into #{} (keep (fn [[o a _]] (when (empty? a) o))) (:opts spec)))

(defn parse-opts
  ([argv] (parse-opts argv #{}))
  ([argv flags]
   (loop [[a & more] argv pos [] opts {}]
     (cond
       (nil? a) {:args pos :opts opts}
       (contains? flags a) (recur more pos (assoc opts a true))
       (str/starts-with? a "--") (recur (rest more) pos (assoc opts a (first more)))
       :else (recur more (conj pos a) opts)))))

(defn run [argv]
  (let [[v & rest-args] argv]
    (if-let [{:keys [fn] :as spec} (by-verb v)]
      (let [parsed (parse-opts rest-args (flags-of spec))]
        (when (< (count (:args parsed)) (count (remove #(str/ends-with? % "...") (:args spec))))
          (throw (ex-info (str "usage: borgi " v " " (str/join " " (:args spec))) {})))
        (fn parsed))
      (do (usage) (when v (throw (ex-info (str "unknown verb: " v) {})))))))
