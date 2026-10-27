(ns borgclj.cache
  (:require [borgclj.bytes :as b]
            [borgclj.mpack :as pack]
            [borgclj.msgpack :as mp])
  (:import [java.nio.file Files Path StandardOpenOption]))

(def ^:const version 1)
(def ^:const ttl 20)

(defn cache-root ^Path []
  (let [env (System/getenv "BORG_CLJ_CACHE_DIR")
        base (cond
               env env
               (.startsWith (System/getProperty "os.name") "Windows")
               (str (or (System/getenv "LOCALAPPDATA")
                        (str (System/getProperty "user.home") "\\AppData\\Local"))
                    "\\borg-imager\\cache")
               :else (str (or (System/getenv "XDG_CACHE_HOME")
                              (str (System/getProperty "user.home") "/.cache"))
                          "/borg-imager"))]
    (Path/of base (into-array String []))))

(defn cache-file ^Path [^String repo-id]
  (.resolve (cache-root) (str repo-id ".cache")))

(defn- ids->blob ^bytes [ids]
  (let [out (byte-array (* 32 (count ids)))]
    (reduce (fn [^long off hx]
              (System/arraycopy (b/unhex hx) 0 out off 32)
              (+ off 32))
            0 ids)
    out))

(defn- blob->ids [blob]
  (let [^bytes bs (b/->bytes blob)
        n (quot (alength bs) 32)]
    (into #{} (map (fn [i] (b/hex (java.util.Arrays/copyOfRange bs (* 32 i) (* 32 (inc i))))))
          (range n))))

(defn empty-cache [repo-id]
  {:repo-id repo-id :manifest-id nil :chunks #{} :files {}})

(defn load-cache [^String repo-id]
  (let [f (cache-file repo-id)]
    (if-not (Files/exists f (into-array java.nio.file.LinkOption []))
      (empty-cache repo-id)
      (try
        (let [m (mp/unpack (Files/readAllBytes f))]
          (if (and (= version (:version m)) (= repo-id (:repo_id m)))
            {:repo-id repo-id
             :manifest-id (:manifest_id m)
             :chunks (blob->ids (:chunks m))
             :files (into {} (map (fn [[p v]]
                                    [(b/key->str p) {:age (nth v 0) :size (nth v 1) :mtime (nth v 2)
                                               :ino (nth v 3)
                                               :chunks (mapv (fn [c] [(b/->bytes (nth c 0)) (nth c 1) (nth c 2)])
                                                             (nth v 4))}]))
                            (:files m))}
            (empty-cache repo-id)))
        (catch Exception _ (empty-cache repo-id))))))

(defn save-cache! [cache]
  (let [f (cache-file (:repo-id cache))]
    (Files/createDirectories (.getParent f) (into-array java.nio.file.attribute.FileAttribute []))
    (let [data (pack/pack {"version" version
                           "repo_id" (:repo-id cache)
                           "manifest_id" (:manifest-id cache)
                           "chunks" (ids->blob (:chunks cache))
                           "files" (into {} (map (fn [[p v]]
                                                   [p [(:age v) (:size v) (:mtime v) (:ino v)
                                                       (mapv (fn [[id sz csz]] [id sz csz]) (:chunks v))]]))
                                         (:files cache))})
          tmp (.resolveSibling f (str (.getFileName f) ".tmp"))]
      (Files/write tmp ^bytes data (into-array StandardOpenOption
                                               [StandardOpenOption/CREATE
                                                StandardOpenOption/TRUNCATE_EXISTING
                                                StandardOpenOption/WRITE]))
      (Files/move tmp f (into-array java.nio.file.CopyOption
                                    [java.nio.file.StandardCopyOption/REPLACE_EXISTING])))
    cache))

(defn hit
  "Cached chunk list for an unchanged file, or nil. Never returns chunks the
   repository does not have."
  [cache ^String path {:keys [size mtime ino]}]
  (when-let [e (get (:files cache) path)]
    (when (and (= (long size) (long (:size e)))
               (= (long mtime) (long (:mtime e)))
               (or (nil? ino) (nil? (:ino e)) (= (long ino) (long (:ino e))))
               (every? (fn [[id _ _]] (contains? (:chunks cache) (b/hex id))) (:chunks e)))
      (:chunks e))))

(defn remember [cache ^String path {:keys [size mtime ino]} chunks]
  (assoc-in cache [:files path] {:age 0 :size size :mtime mtime :ino ino :chunks chunks}))

(defn touch [cache ^String path]
  (if (get-in cache [:files path])
    (assoc-in cache [:files path :age] 0)
    cache))

(defn expire [cache seen-paths]
  (update cache :files
          (fn [files]
            (into {} (keep (fn [[p v]]
                             (cond
                               (contains? seen-paths p) [p (assoc v :age 0)]
                               (< (long (:age v 0)) ttl) [p (update v :age (fnil inc 0))]
                               :else nil)))
                  files))))
