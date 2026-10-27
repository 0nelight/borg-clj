(ns borgclj.win.vss
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.nio.file CopyOption Files LinkOption Path StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

(def device-re #"\\\\\?\\GLOBALROOT\\Device\\HarddiskVolumeShadowCopy\d+")
(def guid-re #"\{[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}\}")

(defn sh
  [argv & {:keys [timeout-s] :or {timeout-s 300}}]
  (let [pb (doto (ProcessBuilder. ^java.util.List (vec argv)) (.redirectErrorStream true))
        p (.start pb)
        out (slurp (.getInputStream p))]
    (if (.waitFor p (long timeout-s) TimeUnit/SECONDS)
      {:exit (.exitValue p) :out out}
      (do (.destroyForcibly p) {:exit -1 :out out :timeout true}))))

(defn- have? [^String exe]
  (try (zero? (:exit (sh [exe "-?"] :timeout-s 20))) (catch Exception _ false)))

(defn- drive-letter [^String v]
  (let [s (-> v
              (str/replace #"^\\\\[.?]\\" "")
              (str/replace #"[\\/]+$" "")
              (str/replace #":$" "")
              str/upper-case)]
    (when-not (re-matches #"[A-Z]" s)
      (throw (ex-info (str "not a drive letter: " v) {:volume v})))
    s))

(defn- drive-arg [^String v] (str (drive-letter v) ":"))

(defn- drive-root [^String v] (str (drive-letter v) ":\\"))

(defn- ps
  "Runs a PowerShell script through -EncodedCommand: the script crosses the
   Windows command line as base64 UTF-16LE, so quoting inside it cannot be
   mangled on the way."
  [script]
  (let [b64 (.encodeToString (java.util.Base64/getEncoder)
                             (.getBytes ^String script java.nio.charset.StandardCharsets/UTF_16LE))]
    (sh ["powershell" "-NoProfile" "-NonInteractive" "-ExecutionPolicy" "Bypass"
         "-EncodedCommand" b64])))

(defn- wmi-release [id]
  (ps (str "Get-CimInstance Win32_ShadowCopy | Where-Object { $_.ID -eq '" id "' }"
           " | Remove-CimInstance")))

(defn release-ids!
  "Releases snapshots by id, with the provider that took them."
  [provider ids]
  (doseq [id ids]
    (try
      (case provider
        :wmi (wmi-release id)
        :vssadmin (sh ["vssadmin" "delete" "shadows" (str "/shadow=" id) "/quiet"] :timeout-s 60)
        :vshadow (when-let [exe (first (filter have? ["vshadow.exe" "vshadow64.exe"]))]
                   (sh [exe (str "-ds=" id)] :timeout-s 60))
        nil)
      (catch Exception _ nil))))

(defn- vshadow-take [volumes]
  (let [exe (first (filter have? ["vshadow.exe" "vshadow64.exe"]))]
    (when exe
      (let [{:keys [exit out]} (sh (into [exe "-p" "-nw"] (map drive-arg volumes)))
            devs (re-seq device-re out)
            ids (re-seq guid-re out)]
        (when (and (zero? exit) (= (count devs) (count volumes)))
          {:provider :vshadow
           :devices (zipmap (map drive-letter volumes) devs)
           :ids (vec ids)
           :atomic true
           :release! (fn [] (release-ids! :vshadow ids))})))))

(defn- wmi-take-one [volume]
  (let [{:keys [exit out]}
        (ps (str "$ErrorActionPreference='Stop'\n"
                 "$r = Invoke-CimMethod -ClassName Win32_ShadowCopy -MethodName Create"
                 " -Arguments @{Volume='" (drive-root volume) "'; Context='ClientAccessible'}\n"
                 "if ($r.ReturnValue -ne 0) { throw ('vss returncode ' + $r.ReturnValue) }\n"
                 "$s = Get-CimInstance Win32_ShadowCopy | Where-Object { $_.ID -eq $r.ShadowID }\n"
                 "Write-Output ($r.ShadowID + '|' + $s.DeviceObject)\n"))]
    (when (zero? exit)
      (when-let [line (first (filter #(str/includes? % "|") (str/split-lines out)))]
        (let [[id dev] (str/split (str/trim line) #"\|" 2)]
          (if (re-matches device-re (or dev ""))
            {:id id :device dev}
            (do (when (re-matches guid-re (or id "")) (wmi-release id)) nil)))))))

(defn- wmi-take [volumes]
  (let [taken (reduce (fn [acc v]
                        (if-let [r (wmi-take-one v)]
                          (assoc acc (drive-letter v) r)
                          (reduced (do (doseq [[_ t] acc] (wmi-release (:id t))) nil))))
                      {} volumes)]
    (when (seq taken)
      {:provider :wmi
       :devices (into {} (map (fn [[k v]] [k (:device v)])) taken)
       :ids (mapv :id (vals taken))
       :atomic (= 1 (count volumes))
       :release! (fn [] (release-ids! :wmi (mapv :id (vals taken))))})))

(defn- vssadmin-take [volumes]
  (let [taken (reduce (fn [acc v]
                        (let [{:keys [exit out]} (sh ["vssadmin" "create" "shadow" (str "/for=" (drive-arg v))])
                              dev (first (re-seq device-re out))
                              id (first (re-seq guid-re out))]
                          (if (and (zero? exit) dev id)
                            (assoc acc (drive-letter v) {:id id :device dev})
                            (reduced nil))))
                      {} volumes)]
    (when (seq taken)
      {:provider :vssadmin
       :devices (into {} (map (fn [[k v]] [k (:device v)])) taken)
       :ids (mapv :id (vals taken))
       :atomic (= 1 (count volumes))
       :release! (fn [] (release-ids! :vssadmin (mapv :id (vals taken))))})))

(def providers
  [[:vshadow  vshadow-take]
   [:wmi      wmi-take]
   [:vssadmin vssadmin-take]])

(defn probe []
  (mapv (fn [[nm _]]
          {:provider nm
           :available (case nm
                        :vshadow (boolean (some have? ["vshadow.exe" "vshadow64.exe"]))
                        :wmi (zero? (:exit (ps "Get-CimClass Win32_ShadowCopy | Out-Null")))
                        :vssadmin (let [{:keys [out]} (sh ["vssadmin" "create" "shadow" "/?"])]
                                    (boolean (re-find #"(?i)create shadow" (or out "")))))})
        providers))

(defn take-snapshot!
  "Snapshots volumes (a seq of drive letters). Returns nil when no provider works."
  [volumes {:keys [provider]}]
  (let [ps* (if provider (filter (fn [[nm _]] (= nm provider)) providers) providers)]
    (some (fn [[_ f]] (try (f volumes) (catch Exception _ nil))) ps*)))

;; ------------------------------------------------------------- what is left

;; A snapshot outlives the run that took it. Nothing inside the process can

(def ^:dynamic *registry*
  "Where taken snapshots are noted. Bound in tests; nil means the default."
  nil)

(defn- registry ^Path []
  (or *registry*
      (let [base (or (System/getenv "ProgramData") (System/getProperty "java.io.tmpdir"))]
        (.resolve (Path/of base (into-array String ["borg-imager"])) "snapshots.edn"))))

(defn recorded
  "The snapshots noted by this and by earlier runs."
  []
  (let [p (registry)]
    (if (Files/exists p (into-array LinkOption []))
      (try (vec (edn/read-string (slurp (.toFile p)))) (catch Exception _ []))
      [])))

(defn- write-registry! [records]
  (let [p (registry)
        tmp (.resolveSibling p (str (.getFileName p) ".neu"))]
    (Files/createDirectories (.getParent p) (into-array FileAttribute []))
    (Files/write tmp (.getBytes (pr-str (vec records)) "UTF-8") (into-array java.nio.file.OpenOption []))
    (Files/move tmp p (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                              StandardCopyOption/ATOMIC_MOVE]))))

(defn remember!
  "Notes a snapshot that has just been taken, together with the process that
   owns it."
  [{:keys [provider ids]} volumes]
  (let [h (java.lang.ProcessHandle/current)]
    (write-registry!
     (conj (recorded)
           {:provider provider
            :ids (vec ids)
            :volumes (vec volumes)
            :pid (.pid h)
            :started (some-> (.startInstant (.info h)) (.orElse nil) (.toEpochMilli))
            :at (str (java.time.Instant/now))}))))

(defn forget! [ids]
  (let [gone (set ids)]
    (write-registry! (remove #(= gone (set (:ids %))) (recorded)))))

(defn owner-alive?
  "Whether the process that took a noted snapshot is still running."
  [{:keys [pid started]}]
  (boolean
   (when pid
     (when-let [h (.orElse (java.lang.ProcessHandle/of (long pid)) nil)]
       (if-let [t (.orElse (.startInstant (.info h)) nil)]
         (or (nil? started) (= (long started) (.toEpochMilli t)))
         true)))))

(defn stale
  "Splits noted snapshots into the ones whose owner is gone and the rest."
  [records alive?]
  [(vec (remove alive? records)) (vec (filter alive? records))])

(defn release-stale!
  "Releases every snapshot noted by a run that is no longer running, and
   forgets it."
  []
  (let [[dead alive] (stale (recorded) owner-alive?)]
    (doseq [r dead] (release-ids! (:provider r) (:ids r)))
    (when (seq dead) (write-registry! alive))
    dead))
