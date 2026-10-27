(ns borgclj.ui.config
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.nio.file Files LinkOption OpenOption Path StandardOpenOption]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def defaults
  {:repo "" :identity "" :user "" :remote-path "" :rsh "" :server-cmd ""
   :compression "zstd,3" :archive "{hostname}-{date}" :block "4M"
   :snapshot "auto" :exclude "" :init false})

(defn config-path ^Path []
  (let [dir (or (System/getenv "APPDATA")
                (System/getenv "XDG_CONFIG_HOME")
                (str (System/getProperty "user.home") "/.config"))]
    (Path/of dir (into-array String ["borg-imager" "ui.edn"]))))

(defn load-config []
  (let [p (config-path)]
    (merge defaults
           (when (Files/exists p (make-array LinkOption 0))
             (edn/read-string (slurp (.toFile p)))))))

(defn save! [cfg]
  (let [p (config-path)]
    (Files/createDirectories (.getParent p) (make-array java.nio.file.attribute.FileAttribute 0))
    (Files/write p (.getBytes (pr-str cfg) "UTF-8")
                 (into-array OpenOption [StandardOpenOption/CREATE
                                         StandardOpenOption/TRUNCATE_EXISTING
                                         StandardOpenOption/WRITE]))))

(defn- hostname ^String []
  (str/lower-case (or (System/getenv "COMPUTERNAME") (System/getenv "HOSTNAME") "host")))

(defn archive-name ^String [pattern]
  (let [now (LocalDateTime/now)]
    (-> (if (str/blank? pattern) (:archive defaults) pattern)
        (str/replace "{hostname}" (hostname))
        (str/replace "{date}" (.format now (DateTimeFormatter/ofPattern "yyyy-MM-dd")))
        (str/replace "{time}" (.format now (DateTimeFormatter/ofPattern "HHmm"))))))

(defn- v ^String [cfg k] (str/trim (str (get cfg k ""))))

(defn location ^String [cfg archive]
  (let [r (v cfg :repo)]
    (when (str/blank? r)
      (throw (ex-info "Kein Repository eingestellt — bitte unter „Ziel“ eintragen." {})))
    (if (str/blank? (str archive)) r (str r "::" (str/trim (str archive))))))

(defn connection-args
  "Value and switch are separate argv entries: the CLI's parser knows no
   --option=value form."
  [cfg]
  (into [] (mapcat (fn [[k flag]] (when (seq (v cfg k)) [flag (v cfg k)])))
        [[:identity "--identity"] [:user "--user"] [:remote-path "--remote-path"]
         [:rsh "--rsh"] [:server-cmd "--server-cmd"]]))

(defn store-opts
  "The same connection settings in the shape the CLI hands to borgclj.store."
  [cfg]
  (apply hash-map (connection-args cfg)))

(defn image-args [cfg {:keys [archive sources dry-run raw no-default-excludes]}]
  (when (empty? sources) (throw (ex-info "Keine Quelle gewählt." {})))
  (-> ["image" (location cfg (if (str/blank? (str archive))
                               (archive-name (:archive cfg))
                               archive))]
      (into sources)
      (cond->
       (seq (v cfg :compression)) (into ["--compression" (v cfg :compression)])
       (seq (v cfg :block)) (into ["--block" (v cfg :block)])
       (and (seq (v cfg :snapshot)) (not= "auto" (v cfg :snapshot)))
       (into ["--snapshot" (v cfg :snapshot)])
       (seq (v cfg :exclude)) (into ["--exclude" (v cfg :exclude)])
       (:init cfg) (conj "--init")
       no-default-excludes (conj "--no-default-excludes")
       raw (conj "--raw")
       dry-run (conj "--dry-run"))
      ;; the interface reads progress from the run itself, so the once-a-second
      ;; line on stderr would only clutter the protocol
      (conj "--quiet")
      (into (connection-args cfg))))

(defn restore-args [cfg {:keys [archive target image partition to-file vhd skip-zeros force]}]
  (when (str/blank? (str archive)) (throw (ex-info "Kein Archiv gewählt." {})))
  (when (str/blank? (str target)) (throw (ex-info "Kein Ziel angegeben." {})))
  (-> ["restore-image" (location cfg archive) "--to" (str/trim target)]
      (cond->
       to-file (conj "--to-file")
       (seq (str image)) (into ["--image" image])
       (seq (str partition)) (into ["--partition" partition])
       vhd (conj "--vhd")
       skip-zeros (conj "--skip-zeros")
       force (conj "--force"))
      (into (connection-args cfg))))

(defn fetch-args [cfg {:keys [archive path dest image partition]}]
  (when (str/blank? (str archive)) (throw (ex-info "Kein Archiv gewählt." {})))
  (when (str/blank? (str path)) (throw (ex-info "Nichts ausgewählt." {})))
  (when (str/blank? (str dest)) (throw (ex-info "Kein Zielordner angegeben." {})))
  (-> ["get" (location cfg archive) (str/trim path) "--to" (str/trim dest)]
      (cond->
       (seq (str image)) (into ["--image" image])
       (seq (str partition)) (into ["--partition" partition]))
      (into (connection-args cfg))))
