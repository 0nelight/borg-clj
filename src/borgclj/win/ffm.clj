(ns borgclj.win.ffm
  (:import [java.lang.invoke MethodHandle]
           [java.lang.foreign Arena FunctionDescriptor Linker Linker$Option
            MemoryLayout MemoryLayout$PathElement MemorySegment SymbolLookup ValueLayout]
           [java.nio.charset StandardCharsets]))

(def ^:const generic-read 0x80000000)
(def ^:const generic-write 0x40000000)
(def ^:const share-read 1)
(def ^:const share-write 2)
(def ^:const open-existing 3)
(def ^:const flag-backup-semantics 0x02000000)
(def ^:const flag-write-through 0x80000000)
(def ^:const invalid-handle -1)

(def ^:const error-more-data 234)
(def ^:const error-handle-eof 38)
(def ^:const error-invalid-function 1)
(def ^:const error-not-supported 50)
(def ^:const error-no-more-files 18)

(def error-hints
  {1 "the device does not support this call"
   5 "access denied -- run this from an elevated (Administrator) console"
   6 "invalid handle"
   21 "the device is not ready"
   32 "sharing violation -- another process holds the volume"
   50 "the filesystem does not support this call"
   87 "invalid parameter -- offset or length is not sector aligned"
   1117 "device I/O error (bad sectors?)"})

(defn windows? []
  (.startsWith (System/getProperty "os.name") "Windows"))

(defn need-windows! [what]
  (when-not (windows?)
    (throw (ex-info (str what " needs Windows") {:os (System/getProperty "os.name")}))))

(def ^:private linker (delay (Linker/nativeLinker)))
(def ^:private k32 (delay (SymbolLookup/libraryLookup "kernel32.dll" (Arena/global))))
(def ^:private state-layout (delay (Linker$Option/captureStateLayout)))

(def ^:private gle-offset
  (delay (.byteOffset ^MemoryLayout @state-layout
                      (into-array MemoryLayout$PathElement
                                  [(MemoryLayout$PathElement/groupElement "GetLastError")]))))

(defn- capture ^MemorySegment [^Arena a]
  (.allocate a ^MemoryLayout @state-layout))

(defn- errno ^long [^MemorySegment cap]
  (bit-and (long (.get cap ValueLayout/JAVA_INT (long @gle-offset))) 0xffffffff))

(defn- descriptor ^FunctionDescriptor [ret params]
  (let [ps (into-array MemoryLayout params)]
    (if ret (FunctionDescriptor/of ret ps) (FunctionDescriptor/ofVoid ps))))

(defn- handle-of [^String nm ret params]
  (delay
    (let [addr (.orElseThrow (.find ^SymbolLookup @k32 nm))]
      (.downcallHandle ^Linker @linker addr (descriptor ret params)
                       (into-array Linker$Option
                                   [(Linker$Option/captureCallState (into-array String ["GetLastError"]))])))))

(def ^:private ptr ValueLayout/ADDRESS)
(def ^:private i32 ValueLayout/JAVA_INT)
(def ^:private i64 ValueLayout/JAVA_LONG)

(def ^:private h-create-file  (handle-of "CreateFileW"     ptr [ptr i32 i32 ptr i32 i32 ptr]))
(def ^:private h-read-file    (handle-of "ReadFile"        i32 [ptr ptr i32 ptr ptr]))
(def ^:private h-write-file   (handle-of "WriteFile"       i32 [ptr ptr i32 ptr ptr]))
(def ^:private h-set-pointer  (handle-of "SetFilePointerEx" i32 [ptr i64 ptr i32]))
(def ^:private h-ioctl        (handle-of "DeviceIoControl" i32 [ptr i32 ptr i32 ptr i32 ptr ptr]))
(def ^:private h-close        (handle-of "CloseHandle"     i32 [ptr]))
(def ^:private h-flush        (handle-of "FlushFileBuffers" i32 [ptr]))
(def ^:private h-logical      (handle-of "GetLogicalDrives" i32 []))
(def ^:private h-file-info    (handle-of "GetFileInformationByHandleEx" i32 [ptr i32 ptr i32]))

(defn- invoke [^MethodHandle h args]
  (.invokeWithArguments h ^java.util.List (vec args)))

(defn- ok? [r] (not (zero? (long r))))

(defn fail! [^String what ^long code]
  (throw (ex-info (format "%s failed: windows error %d%s" what code
                          (if-let [h (error-hints code)] (str " -- " h) ""))
                  {:win-error code})))

(defn- wide ^MemorySegment [^Arena a ^String s]
  (.allocateFrom a s StandardCharsets/UTF_16LE))

(defrecord Device [^MemorySegment h ^Arena arena ^MemorySegment cap scratch ^String name write?]
  java.lang.AutoCloseable
  (close [_]
    (invoke @h-close [cap h])
    (.close arena)))

(defn open
  (^Device [^String name] (open name {}))
  (^Device [^String name {:keys [write share] :or {share (bit-or share-read share-write)}}]
   (need-windows! "raw device access")
   (let [a (Arena/ofShared)
         cap (capture a)
         access (unchecked-int (if write (bit-or generic-read generic-write) generic-read))
         seg (invoke @h-create-file
                     [cap (wide a name) access (unchecked-int share) MemorySegment/NULL
                      (unchecked-int open-existing) (unchecked-int flag-backup-semantics) MemorySegment/NULL])
         addr (.address ^MemorySegment seg)]
     (when (or (= addr -1) (= addr 0xffffffffffffffff))
       (let [e (errno cap)] (.close a) (fail! (str "open " name) e)))
     (->Device seg a cap (atom nil) name (boolean write)))))

(defn- scratch-of ^MemorySegment [^Device d ^long n]
  (let [cur @(:scratch d)]
    (if (and cur (>= (.byteSize ^MemorySegment cur) n))
      cur
      (reset! (:scratch d) (.allocate ^Arena (:arena d) n 8)))))

(defn seek! [^Device d ^long off]
  (when-not (ok? (invoke @h-set-pointer [(:cap d) (:h d) off MemorySegment/NULL (unchecked-int 0)]))
    (fail! (str "seek " (:name d)) (errno (:cap d)))))

(defn read-at!
  "Reads len bytes at off into dst[0..len). Returns the number of bytes read."
  ^long [^Device d ^long off ^bytes dst ^long len]
  (seek! d off)
  (let [buf (scratch-of d len)
        cap ^MemorySegment (:cap d)]
    (with-open [tmp (Arena/ofConfined)]
      (let [got (.allocate tmp i32)
            done (loop [done 0]
                   (if (>= done len)
                     done
                     (let [want (- len done)
                           r (invoke @h-read-file [cap (:h d) (.asSlice buf (long done))
                                                   (unchecked-int want) got MemorySegment/NULL])]
                       (when-not (ok? r) (fail! (str "read " (:name d) " @" (+ off done)) (errno cap)))
                       (let [n (bit-and (long (.get got i32 0)) 0xffffffff)]
                         (if (zero? n) done (recur (+ done n)))))))]
        (when (pos? done) (MemorySegment/copy buf ValueLayout/JAVA_BYTE 0 dst 0 (int done)))
        (when (< done len)
          (throw (ex-info (format "short read on %s: wanted %d at %d, got %d" (:name d) len off done)
                          {:device (:name d) :offset off})))
        done))))

(defn write-at! [^Device d ^long off ^bytes src ^long len]
  (seek! d off)
  (let [buf (scratch-of d len)
        cap ^MemorySegment (:cap d)]
    (MemorySegment/copy src 0 buf ValueLayout/JAVA_BYTE 0 (int len))
    (with-open [tmp (Arena/ofConfined)]
      (let [done* (.allocate tmp i32)]
        (loop [done 0]
          (when (< done len)
            (let [r (invoke @h-write-file
                            [cap (:h d) (.asSlice buf (long done)) (unchecked-int (- len done)) done* MemorySegment/NULL])]
              (when-not (ok? r) (fail! (str "write " (:name d) " @" (+ off done)) (errno cap)))
              (let [n (bit-and (long (.get done* i32 0)) 0xffffffff)]
                (when (zero? n)
                  (throw (ex-info (str "write stalled on " (:name d)) {:offset (+ off done)})))
                (recur (+ done n))))))))
    len))

(defn flush! [^Device d]
  (invoke @h-flush [(:cap d) (:h d)]))

(defn ioctl
  "Runs DeviceIoControl. Returns {:bytes n :out byte-array} or {:error code}
   when tolerate contains the error code."
  [^Device d code ^bytes in out-size & {:keys [tolerate] :or {tolerate #{}}}]
  (with-open [a (Arena/ofConfined)]
    (let [code (long code)
          out-size (long out-size)
          cap (capture a)
          in-seg (if (and in (pos? (alength in)))
                   (let [s (.allocate a (long (alength in)) 8)]
                     (MemorySegment/copy in 0 s ValueLayout/JAVA_BYTE 0 (alength in))
                     s)
                   MemorySegment/NULL)
          in-len (if in (alength in) 0)
          out-seg (if (pos? out-size) (.allocate a out-size 8) MemorySegment/NULL)
          ret (.allocate a i32)
          r (invoke @h-ioctl [cap (:h d) (unchecked-int code) in-seg (unchecked-int in-len)
                              out-seg (unchecked-int out-size) ret MemorySegment/NULL])
          n (bit-and (long (.get ret i32 0)) 0xffffffff)]
      (if (ok? r)
        {:bytes n :out (let [b (byte-array out-size)]
                         (when (pos? out-size)
                           (MemorySegment/copy out-seg ValueLayout/JAVA_BYTE 0 b 0 (int out-size)))
                         b)}
        (let [e (errno cap)]
          (if (contains? tolerate e)
            {:error e :bytes n :out (let [b (byte-array out-size)]
                                      (when (pos? out-size)
                                        (MemorySegment/copy out-seg ValueLayout/JAVA_BYTE 0 b 0 (int out-size)))
                                      b)}
            (fail! (format "ioctl 0x%08x on %s" code (:name d)) e)))))))

(defn file-info-by-handle
  "GetFileInformationByHandleEx into a fresh byte array, or nil at the end of
   the enumeration."
  [^MemorySegment h ^long klass ^long size]
  (with-open [a (Arena/ofConfined)]
    (let [cap (capture a)
          buf (.allocate a size 8)
          r (invoke @h-file-info [cap h (unchecked-int klass) buf (unchecked-int size)])]
      (if (ok? r)
        (let [b (byte-array size)]
          (MemorySegment/copy buf ValueLayout/JAVA_BYTE 0 b 0 (int size))
          b)
        (let [e (errno cap)]
          (when-not (or (= e error-no-more-files) (= e 38) (= e 1) (= e 50) (= e 5) (= e 87))
            (fail! "GetFileInformationByHandleEx" e))
          nil)))))

(defn close-handle [^MemorySegment h]
  (with-open [a (Arena/ofConfined)]
    (invoke @h-close [(capture a) h])))

(defn logical-drives []
  (need-windows! "drive enumeration")
  (with-open [a (Arena/ofConfined)]
    (let [cap (capture a)
          mask (bit-and (long (invoke @h-logical [cap])) 0xffffffff)]
      (vec (for [i (range 26) :when (pos? (bit-and mask (bit-shift-left 1 i)))]
             (str (char (+ (int \A) i)) ":"))))))
