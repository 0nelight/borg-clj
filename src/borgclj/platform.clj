(ns borgclj.platform
  (:require [borgclj.bytes :as b])
  (:import [java.lang ProcessHandle]
           [java.net InetAddress]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file FileStore Files FileSystems LinkOption Path StandardOpenOption]
           [java.nio.file.attribute AclFileAttributeView FileTime PosixFileAttributeView
            UserDefinedFileAttributeView]))

(def ^:private no-links (into-array LinkOption []))
(def ^:private nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn views []
  (set (map keyword (.supportedFileAttributeViews (FileSystems/getDefault)))))

(defn supports? [^Path p view]
  (.supportsFileAttributeView (Files/getFileStore p) ^String (name view)))

(defn- udv ^UserDefinedFileAttributeView [^Path p follow?]
  (Files/getFileAttributeView p UserDefinedFileAttributeView (if follow? no-links nofollow)))

(defn listxattr [^Path p & {:keys [follow-symlinks] :or {follow-symlinks false}}]
  (if-let [v (udv p follow-symlinks)]
    (vec (.list v))
    []))

(defn getxattr ^bytes [^Path p ^String nm & {:keys [follow-symlinks] :or {follow-symlinks false}}]
  (let [v (udv p follow-symlinks)
        bb (ByteBuffer/allocate (.size v nm))]
    (.read v nm bb)
    (.array bb)))

(defn setxattr [^Path p ^String nm ^bytes value & {:keys [follow-symlinks] :or {follow-symlinks false}}]
  (.write (udv p follow-symlinks) nm (ByteBuffer/wrap value)))

(defn stat [^Path p]
  (let [vs (views)]
    (merge
      (into {} (for [[k v] (Files/readAttributes p "basic:*" nofollow)] [(keyword k) v]))
      (when (:unix vs)
        (into {} (for [[k v] (Files/readAttributes p "unix:mode,uid,gid,nlink,ino,dev,ctime" nofollow)]
                   [(keyword k) v])))
      (when (:dos vs)
        (into {} (for [[k v] (Files/readAttributes p "dos:hidden,system,archive,readonly" nofollow)]
                   [(keyword (str "dos:" k)) v]))))))

(defn acl-get [^Path p]
  (when-let [v (Files/getFileAttributeView p AclFileAttributeView no-links)]
    (mapv (fn [e] {:principal (.getName (.principal e))
                   :type (str (.type e))
                   :perms (mapv str (.permissions e))
                   :flags (mapv str (.flags e))})
          (.getAcl v))))

(defn acl-supported? [^Path p]
  (boolean (and (supports? p :acl)
                (Files/getFileAttributeView p AclFileAttributeView no-links))))

(defn sync-dir [^Path p]
  (with-open [ch (FileChannel/open p (into-array StandardOpenOption [StandardOpenOption/READ]))]
    (.force ch true)))

(defn local-pid-alive? [^long pid]
  (.isPresent (ProcessHandle/of pid)))

(defn process-alive? [^String host ^long pid]
  (if (= (.toLowerCase (or (first (.split host "@")) "")) (.toLowerCase (.getHostName (InetAddress/getLocalHost))))
    (local-pid-alive? pid)
    true))

(defn fqdn []
  (.getCanonicalHostName (InetAddress/getLocalHost)))

(def wide-ranges
  [[0x1100 0x115f] [0x2e80 0xa4cf] [0xac00 0xd7a3] [0xf900 0xfaff]
   [0xfe30 0xfe6f] [0xff00 0xff60] [0xffe0 0xffe6] [0x20000 0x3fffd]])

(defn- wide? [^long cp]
  (boolean (some (fn [[lo hi]] (<= (long lo) cp (long hi))) wide-ranges)))

(defn swidth ^long [^String s]
  (reduce (fn [^long acc ^long cp]
            (cond
              (= 0 (Character/getType (int cp))) acc
              (wide? cp) (+ acc 2)
              :else (inc acc)))
          0 (iterator-seq (.iterator (.codePoints s)))))

(defn apply-metadata! [^Path p it]
  (when-let [mt (:mtime it)]
    (Files/setLastModifiedTime p (FileTime/fromMillis (long (/ (long mt) 1000000)))))
  (when (and (:mode it) (:posix (views)))
    (let [m (bit-and (long (:mode it)) 0777)]
      (Files/setPosixFilePermissions
        p (java.nio.file.attribute.PosixFilePermissions/fromString
            (apply str (for [[bit ch] [[0400 "r"] [0200 "w"] [0100 "x"] [040 "r"] [020 "w"] [010 "x"] [04 "r"] [02 "w"] [01 "x"]]]
                         (if (pos? (bit-and m (long bit))) ch "-")))))))
  p)

(defn capabilities []
  (let [tmp (Files/createTempDirectory "borgclj" (into-array java.nio.file.attribute.FileAttribute []))
        f (Files/createFile (.resolve tmp "probe") (into-array java.nio.file.attribute.FileAttribute []))]
    (try
      {:os (System/getProperty "os.name")
       :arch (System/getProperty "os.arch")
       :views (sort (map name (views)))
       :xattr (boolean (try (setxattr f "user.borgclj" (.getBytes "1")) (seq (listxattr f))
                            (catch Throwable _ false)))
       :acl (acl-supported? f)
       :posix-mode (contains? (views) :posix)
       :unix-stat (contains? (views) :unix)
       :dos-attrs (contains? (views) :dos)
       :hardlinks (boolean (try (Files/createLink (.resolve tmp "hl") f) true (catch Throwable _ false)))
       :symlinks (boolean (try (Files/createSymbolicLink (.resolve tmp "sl") f
                                                         (into-array java.nio.file.attribute.FileAttribute []))
                               true (catch Throwable _ false)))
       :file-lock (boolean (try (with-open [ch (FileChannel/open f (into-array StandardOpenOption [StandardOpenOption/WRITE]))]
                                  (.release (.tryLock ch)) true)
                                (catch Throwable _ false)))
       :process-handle (local-pid-alive? (.pid (ProcessHandle/current)))}
      (finally
        (doseq [^Path x (reverse (vec (iterator-seq (.iterator (Files/walk tmp (into-array java.nio.file.FileVisitOption []))))))]
          (try (Files/deleteIfExists x) (catch Throwable _ nil)))))))
