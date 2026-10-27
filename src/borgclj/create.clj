(ns borgclj.create
  (:require [borgclj.bytes :as b]
            [borgclj.cache :as cache]
            [borgclj.chunker :as chunker]
            [borgclj.image :as image]
            [borgclj.key :as key]
            [borgclj.mpack :as pack]
            [borgclj.msgpack :as mp]
            [borgclj.platform :as plat]
            [borgclj.remote :as remote]
            [borgclj.source :as src]
            [clojure.string :as str])
  (:import [java.nio.file Files LinkOption Path]
           [java.security SecureRandom]
           [java.time Instant ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def manifest-id (byte-array 32))
(def item-keys
  ["path" "source" "rdev" "chunks" "chunks_healthy" "hardlink_master" "mode" "user" "group"
   "uid" "gid" "mtime" "atime" "ctime" "birthtime" "size" "xattrs" "bsdflags"
   "acl_nfs4" "acl_access" "acl_default" "acl_extended" "part"])

(def buzhash-params ["buzhash" 19 23 21 4095])

(defn fixed-params [^long block] ["fixed" block 0])

(def ^:private iso (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSSSS"))

(defn now-iso []
  (.format (.atZone (Instant/now) ZoneOffset/UTC) iso))

(defn- ns-time ^long [attrs k]
  (if-let [^java.nio.file.attribute.FileTime t (get attrs k)]
    (let [i (.toInstant t)]
      (+ (* 1000000000 (.getEpochSecond i)) (.getNano i)))
    0))

(defn- nofollow [] (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- entry-type [attrs]
  (cond
    (get attrs "isSymbolicLink") :symlink
    (get attrs "isDirectory") :dir
    (get attrs "isRegularFile") :file
    :else :other))

(defn- principal-name [attrs k]
  (when-let [p (get attrs k)]
    (.getName ^java.nio.file.attribute.UserPrincipal p)))

(defn fallback-mode ^long [attrs t]
  (case t
    :dir 040755
    :symlink 0120777
    :file (if (get attrs "readonly") 0100444 0100644)
    0100644))

(defn rel-path ^String [^Path root ^Path p]
  (let [base (or (.getParent (.toAbsolutePath root)) (.toAbsolutePath root))
        r (str (.relativize base (.toAbsolutePath p)))]
    (str/replace r "\\" "/")))

(defn item-for [^Path root ^Path p]
  (let [basic (into {} (Files/readAttributes p "basic:*" (nofollow)))
        unix (try (into {} (Files/readAttributes p "unix:mode,uid,gid,ctime,lastAccessTime,owner,group" (nofollow)))
                  (catch Exception _ {}))
        dos (try (into {} (Files/readAttributes p "dos:readonly,hidden,system,archive" (nofollow)))
                 (catch Exception _ {}))
        attrs (merge basic dos unix)
        rel (rel-path root p)
        t (entry-type attrs)]
    (cond-> {:path rel
             :mtime (ns-time attrs "lastModifiedTime")
             :atime (ns-time attrs "lastAccessTime")
             :ctime (or (some-> (get unix "ctime") (as-> ft (ns-time {"c" ft} "c"))) 0)}
      true (assoc :mode (long (or (get attrs "mode") (fallback-mode attrs t))))
      true (assoc :uid (long (or (get attrs "uid") 0)))
      true (assoc :gid (long (or (get attrs "gid") 0)))
      true (assoc :user (or (principal-name attrs "owner")
                            (System/getProperty "user.name")
                            "unknown"))
      true (assoc :group (or (principal-name attrs "group")
                             (principal-name attrs "owner")
                             (System/getProperty "user.name")
                             "unknown"))
      (= :symlink t) (assoc :source (b/latin1 (b/unlatin1 (str (Files/readSymbolicLink p)))))
      (= :file t) (assoc ::file p
                         ::size (long (get attrs "size"))
                         ::ino (when-let [i (get unix "ino")] (long i))))))

(defn walk-items [^Path root]
  (let [out (volatile! [])]
    (letfn [(visit [^Path p]
              (let [attrs (into {} (Files/readAttributes p "basic:*" (nofollow)))]
                (vswap! out conj (item-for root p))
                (when (and (get attrs "isDirectory") (not (get attrs "isSymbolicLink")))
                  (doseq [^Path c (sort-by str (vec (iterator-seq (.iterator (Files/newDirectoryStream p)))))]
                    (visit c)))))]
      (visit (.toAbsolutePath root)))
    @out))

(defn- put-chunk!
  ([conn k seen ^bytes data] (put-chunk! conn k seen data (alength data)))
  ([conn k seen ^bytes data len]
   (let [len (long len)
         payload (if (= len (alength data)) data (java.util.Arrays/copyOfRange data 0 (int len)))
         id (key/id-hash k payload)
         hx (b/hex id)
         fresh (not (contains? @seen hx))
         ^bytes env (when fresh (key/encrypt k payload))]
     (when fresh
       (remote/put! conn id env)
       (vswap! seen conj hx))
     [[id (alength payload) (if env (alength env) (alength payload))] fresh])))

(defn- pack-metadata ^bytes [k m ^String context]
  (let [salt (byte-array 64)
        _ (.nextBytes (SecureRandom.) salt)
        base (assoc m :tam {:type "HKDF_HMAC_SHA512" :hmac (byte-array 64) :salt salt})
        packed0 (pack/pack base)
        mac (key/hmac-sha512 (key/tam-key k salt context) packed0)]
    (pack/pack (assoc base :tam {:type "HKDF_HMAC_SHA512" :hmac mac :salt salt}))))

(defn- manifest-plain [conn k]
  (let [raw (try (remote/get-object conn manifest-id)
                 (catch Exception e
                   (when-not (re-find #"ObjectNotFound|DoesNotExist" (str (ex-message e)))
                     (throw e))
                   nil))]
    (when raw (key/decrypt k nil raw))))

(defn init-repo!
  "Finishes what our --init used to leave half done."
  [conn k]
  (when-not (manifest-plain conn k)
    (let [mdata (pack-metadata k {:version 1
                                  :archives {}
                                  :timestamp (now-iso)
                                  :config {}
                                  :item_keys item-keys}
                               "manifest")]
      (remote/put! conn manifest-id (key/encrypt k mdata))
      (remote/commit! conn)
      true)))

(defn begin
  "Reads the manifest and the client-side chunk cache, so that only chunks the
   repository does not already hold are ever put."
  [conn k repo-id]
  (let [plain (manifest-plain conn k)
        old (if plain (mp/unpack plain) {:archives {} :config {}})
        old-mid (when plain (b/hex (key/id-hash k plain)))
        cached (if repo-id (cache/load-cache repo-id) (cache/empty-cache "-"))
        cache0 (if (and old-mid (= old-mid (:manifest-id cached)))
                 cached
                 (assoc cached :chunks (into #{} (map b/hex) (remote/ids conn))))]
    {:old old :cache cache0 :seen (volatile! (:chunks cache0)) :cache* (volatile! cache0)}))

(defn refuse-taken!
  "borg refuses an archive name that is already in the manifest, and so does
   this: the manifest maps names to archives, and writing the same name again
   would silently drop the older archive from it -- its chunks stay in the
   repository, but nothing reaches them any more."
  [old ^String nm]
  (when (some #(let [k (b/key->str %)] (or (= nm k) (= nm (b/display k))))
              (keys (:archives old)))
    (throw (ex-info (str "archive " nm " already exists in this repository -- pick another name")
                    {:archive nm}))))

(defn finish!
  "Writes the item stream, the archive and the manifest, then commits."
  [conn k ^String archive-name prepared seen {:keys [old cmdline chunker-params stats]}]
  (refuse-taken! old archive-name)
  (let [item-stream (let [bos (java.io.ByteArrayOutputStream.)]
                      (doseq [it prepared] (.write bos ^bytes (pack/pack it)))
                      (.toByteArray bos))
        item-ids (mapv (fn [c] (first (first (put-chunk! conn k seen c))))
                       (chunker/chunk-stream item-stream (long (:chunk-seed k 0)) chunker/items-params))
        meta {:version 1
              :name archive-name
              :comment ""
              :items item-ids
              :cmdline (or cmdline ["borg" "create" (str archive-name)])
              :hostname (plat/fqdn)
              :username (System/getProperty "user.name")
              :time (:started stats)
              :time_end (now-iso)
              :chunker_params (or chunker-params buzhash-params)
              :size (:size stats 0)
              :csize (:csize stats 0)
              :nfiles (:nfiles stats 0)
              :size_parts 0 :csize_parts 0 :nfiles_parts 0}
        adata (pack-metadata k meta "archive")
        aid (key/id-hash k adata)
        _ (remote/put! conn aid (key/encrypt k adata))
        archives (assoc (into {} (map (fn [[nm v]] [(b/key->str nm) v]) (:archives old)))
                        archive-name {:id aid :time (:time meta)})
        mdata (pack-metadata k {:version 1
                                :archives archives
                                :timestamp (now-iso)
                                :config (or (:config old) {})
                                :item_keys item-keys}
                             "manifest")]
    (remote/put! conn manifest-id (key/encrypt k mdata))
    (remote/commit! conn)
    {:manifest-id (b/hex (key/id-hash k mdata)) :items (count prepared)}))

(defn create!
  ([conn k archive-name roots] (create! conn k archive-name roots nil))
  ([conn k ^String archive-name roots repo-id]
   (let [start-ns (* 1000000 (System/currentTimeMillis))
         started (now-iso)
         {:keys [old seen cache*]} (begin conn k repo-id)
         _ (refuse-taken! old archive-name)
         stats (volatile! {:size 0 :csize 0 :nfiles 0 :reused 0 :read 0 :started started})
         items (vec (mapcat (fn [r] (walk-items r)) roots))
         prepared
         (mapv (fn [it]
                 (if-let [^Path f (::file it)]
                   (let [fmeta {:size (::size it) :mtime (:mtime it) :ino (::ino it)}
                         reused (when repo-id (cache/hit @cache* (:path it) fmeta))
                         cs (or reused
                                (mapv (fn [c] (first (put-chunk! conn k seen c)))
                                      (chunker/chunks f (long (:chunk-seed k 0)))))]
                     (when (and repo-id (nil? reused) (< (long (:mtime it)) start-ns))
                       (vswap! cache* cache/remember (:path it) fmeta cs))
                     (vswap! stats (fn [s] (-> s
                                               (update :size + (reduce + 0 (map second cs)))
                                               (update :csize + (reduce + 0 (map #(nth % 2) cs)))
                                               (update :nfiles inc)
                                               (update (if reused :reused :read) inc))))
                     (-> it (dissoc ::file ::size ::ino) (assoc :chunks cs)))
                   (dissoc it ::file ::size ::ino)))
               items)
         res (finish! conn k archive-name prepared seen
                      {:old old :stats @stats
                       :cmdline ["borg" "create" (str archive-name)]})]
     (when repo-id
       (cache/save-cache! (-> @cache*
                              (assoc :chunks @seen)
                              (assoc :manifest-id (:manifest-id res))
                              (cache/expire (into #{} (keep (fn [it] (when (::file it) (:path it))) items))))))
     {:archive archive-name
      :nfiles (:nfiles @stats)
      :size (:size @stats)
      :csize (:csize @stats)
      :items (:items res)
      :reused (:reused @stats)
      :read (:read @stats)
      :chunks (count @seen)})))

(defn- image-item [^String path ^long mode chunks]
  (let [now (* 1000000 (System/currentTimeMillis))]
    {:path path :mode mode :uid 0 :gid 0
     :user (or (System/getProperty "user.name") "unknown")
     :group (or (System/getProperty "user.name") "unknown")
     :mtime now :atime now :ctime now
     :chunks chunks}))

(defn create-image!
  "Chunks one or more block sources into an archive."
  [conn k ^String archive-name sources
   {:keys [repo-id block-size progress extra stop?]
    :or {block-size image/default-block stop? (constantly false)}}]
  (let [started (now-iso)
        {:keys [old seen cache*]} (begin conn k repo-id)
        _ (refuse-taken! old archive-name)
        stats (volatile! {:size 0 :csize 0 :nfiles 0 :started started
                          :read 0 :zeroed 0 :new-chunks 0 :sent 0 :blocks 0
                          :copy-nanos 0 :zero-nanos 0})
        buf (byte-array block-size)
        ;; every fully zeroed block of a given length has the same chunk id, so
        zeros (volatile! {})
        per-source
        (mapv
          (fn [source]
            (let [p (image/plan source block-size)
                  n (long (:count p))
                  chunks (transient [])]
              (dotimes [i n]
                (when (stop?)
                  ;; thrown, not returned, and on purpose: unwinding past
                  (throw (ex-info "stopped on request"
                                  {:stopped true :source (src/s-name source) :block i :of n})))
                (let [[off len] (nth (:blocks p) i)
                      len (long len)
                      t0 (System/nanoTime)
                      known (when (image/hole? p (long off) len) (get @zeros len))
                      ;; a known hole is answered from the table alone: no fill
                      [kind got] (if known [:zero 0]
                                     (image/read-block! source p buf (long off) len))
                      [c fresh] (if known
                                  [known false]
                                  (let [r (put-chunk! conn k seen buf len)]
                                    (when (zero? (long got))
                                      (vswap! zeros assoc len (first r)))
                                    r))
                      ;; the two regimes run orders of magnitude apart — reading
                      spent (- (System/nanoTime) t0)]
                  (conj! chunks c)
                  (vswap! stats
                          (fn [s] (cond-> (-> s
                                              (update :size + len)
                                              (update :csize + len)
                                              (update :blocks inc)
                                              (update :read + (long got))
                                              (update :zeroed + (- len (long got)))
                                              (update (if (zero? (long got)) :zero-nanos :copy-nanos)
                                                      + spent))
                                    fresh (-> (update :new-chunks inc)
                                              (update :sent + (long (nth c 2)))))))
                  (when progress (progress {:source (src/s-name source) :block i :of n
                                            :length (:length p) :allocated (:allocated p)
                                            :kind kind :fresh fresh :stats @stats}))))
              {:source source :plan p
               :item (image-item (image/image-name source) 0100644 (persistent! chunks))}))
          sources)
        info {:tool "borg-imager image"
              :created started
              :block-size block-size
              :free-space :zeroed
              :sources (mapv (fn [{:keys [source plan item]}]
                               (merge (src/s-info source)
                                      {:image (:path item)
                                       :length (src/s-length source)
                                       :blocks (:count plan)
                                       :cuts (:cuts plan)
                                       :chunking (if (:aligned plan) :run-relative :fixed)
                                       :allocated-bytes (:allocated plan)
                                       :extents (src/s-extents source)}))
                             per-source)}
        info (merge info extra)
        meta-bytes (.getBytes ^String (pr-str info) "UTF-8")
        meta-chunks (mapv (fn [c] (first (put-chunk! conn k seen c)))
                          (chunker/chunk-stream meta-bytes (long (:chunk-seed k 0)) chunker/items-params))
        prepared (conj (mapv :item per-source) (image-item "imager.edn" 0100644 meta-chunks))
        res (finish! conn k archive-name prepared seen
                     {:old old :stats @stats
                      :chunker-params (fixed-params block-size)
                      :cmdline (into ["borg" "image" (str archive-name)] (map src/s-name sources))})]
    (when repo-id
      (cache/save-cache! (-> @cache*
                             (assoc :chunks @seen)
                             (assoc :manifest-id (:manifest-id res))
                             (cache/expire #{}))))
    (assoc @stats :archive archive-name :items (:items res) :chunks (count @seen) :info info)))
