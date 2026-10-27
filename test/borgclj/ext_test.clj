(ns borgclj.ext-test
  "Prueft den ext-Leser gegen echte Abbilder, nicht gegen Erwartungen."
  (:require [borgclj.ext :as ext]
            [borgclj.source :as src]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files Path]))

(defn- tool [^String n]
  (first (filter #(.canExecute (io/file %))
                 [(str "/usr/sbin/" n) (str "/sbin/" n) (str "/usr/bin/" n)])))

(defn- tmpdir ^Path []
  (Files/createTempDirectory "borgclj-ext" (into-array java.nio.file.attribute.FileAttribute [])))

(def ^:private big-name "gross.bin")
(def ^:private big-size (* 8 1024 1024))

(defn- make-tree! [^Path dir]
  (let [root (.resolve dir "fsroot")]
    (Files/createDirectories (.resolve root "unterordner")
                             (into-array java.nio.file.attribute.FileAttribute []))
    ;; Ein Muster statt Zufall: der Test kann den Inhalt spaeter ohne die
    (with-open [os (io/output-stream (.toFile (.resolve root big-name)))]
      (let [buf (byte-array 4096)]
        (dotimes [i (quot big-size 4096)]
          (dotimes [j 4096] (aset-byte buf j (unchecked-byte (rem (+ (* i 7) j) 251))))
          (.write os buf))))
    (dotimes [n 12]
      (spit (.toFile (.resolve root (format "klein-%02d.txt" n)))
            (str/join (repeat 40 (str "kleine datei " n "\n")))))
    (spit (.toFile (.resolve root "unterordner/tief.txt")) (str/join (repeat 1000 "tief\n")))
    root))

(defn- mkfs! [^Path dir ^String img ^String type block-size bytes- ^Path root]
  (let [p (str (.resolve dir img))]
    (with-open [raf (java.io.RandomAccessFile. p "rw")] (.setLength raf bytes-))
    (let [{:keys [exit err]} (sh/sh (tool "mke2fs") "-q" "-t" type "-b" (str block-size)
                                    "-d" (str root) "-F" p)]
      (when-not (zero? exit) (throw (ex-info (str "mke2fs: " err) {})))
      p)))

(defn- dumpe2fs-header [^String img]
  (into {}
        (keep (fn [line]
                (when-let [[_ k v] (re-matches #"^([^:]+):\s+(.*)$" line)]
                  [(str/trim k) (str/trim v)])))
        (str/split-lines (:out (sh/sh (tool "dumpe2fs") "-h" img)))))

(defn- debugfs-first-block ^long [^String img ^String file]
  (-> (sh/sh (tool "debugfs") "-R" (str "blocks /" file) img)
      :out str/trim (str/split #"\s+") first parse-long))

(defn- file-source-over [^String img]
  (src/file-source (Path/of img (into-array String []))))

(deftest ext-belegung-deckt-sich-mit-dumpe2fs
  (if-not (and (tool "mke2fs") (tool "dumpe2fs") (tool "debugfs"))
    (println "  (kein e2fsprogs auf diesem Host -- ext-Gegenprobe uebersprungen)")
    (let [dir (tmpdir)
          root (make-tree! dir)]
      (try
        (doseq [[img type block-size bytes-]
                [["ext4-4k.img" "ext4" 4096 (* 192 1024 1024)]
                 ["ext3-1k.img" "ext3" 1024 (* 32 1024 1024)]]]
          (testing (str type " mit " block-size "-Byte-Bloecken")
            (let [p (mkfs! dir img type block-size bytes- root)
                  hdr (dumpe2fs-header p)
                  blocks (parse-long (get hdr "Block count"))
                  free (parse-long (get hdr "Free blocks"))
                  s (file-source-over p)]
              (try
                (let [es (ext/open-partition-source s 0 (src/s-length s) "test" {})]
                  (is (some? es) "ein ext-Dateisystem muss erkannt werden")
                  (let [info (src/s-info es)]
                    (is (= block-size (:block-size info)))
                    (is (= blocks (:blocks info)))
                    (is (= (= "ext4" type) (boolean (:extents? info)))
                        "nur ext4 traegt das extent-Merkmal"))

                  (testing "Stufe 1: jeder belegte Block ist dabei, kein freier zu viel"
                    ;; Der Kopf (1 MB Superblock-Bereich) kann Bloecke doppelt
                    (is (= (- blocks free)
                           (quot (src/total (src/s-extents es)) block-size))
                        "belegte Bloecke laut Bitmap == blocks - free laut dumpe2fs"))

                  (testing "Stufe 2: ein Schnitt liegt genau am Anfang der grossen Datei"
                    (let [want (* (debugfs-first-block p big-name) block-size)]
                      (is (contains? (set (src/s-cuts es)) want)
                          (str "Schnitt bei " want " erwartet"))))

                  (testing "an diesem Schnitt stehen wirklich die ersten Bytes der Datei"
                    (let [want (* (debugfs-first-block p big-name) block-size)
                          buf (byte-array 4096)
                          _ (src/s-read es want buf 4096)
                          soll (byte-array 4096)]
                      (dotimes [j 4096] (aset-byte soll j (unchecked-byte (rem j 251))))
                      (is (java.util.Arrays/equals buf soll)
                          "der erste Block der Datei, am errechneten Versatz gelesen")))

                  (testing "der freie Teil wird gar nicht erst gelesen"
                    (is (< (src/total (src/s-extents es)) (* 0.6 (src/s-length es)))
                        "ein halbleeres Abbild darf nicht vollstaendig gelesen werden")))
                (finally (src/s-close s))))))
        (finally
          (doseq [f (reverse (file-seq (.toFile dir)))] (.delete f)))))))

(deftest kein-ext-wird-nicht-erkannt
  (let [dir (tmpdir)
        p (str (.resolve dir "leer.img"))]
    (try
      (with-open [raf (java.io.RandomAccessFile. p "rw")] (.setLength raf (* 8 1024 1024)))
      (let [s (file-source-over p)]
        (try
          (is (nil? (ext/open-partition-source s 0 (src/s-length s) "leer" {}))
              "eine Platte voller Nullen ist kein ext-Dateisystem")
          (finally (src/s-close s))))
      (finally
        (doseq [f (reverse (file-seq (.toFile dir)))] (.delete f))))))
