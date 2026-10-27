(ns borgclj.ntfs-test
  "Prueft den NTFS-Leser gegen echte Abbilder, nicht gegen Erwartungen."
  (:require [borgclj.ntfs :as ntfs]
            [borgclj.source :as src]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files Path]))

(defn- tool [^String n]
  (first (filter #(.canExecute (io/file %))
                 [(str "/usr/sbin/" n) (str "/sbin/" n) (str "/usr/bin/" n)])))

(defn- tools? [& ns] (every? tool ns))

(defn- tmpdir ^Path []
  (Files/createTempDirectory "borgclj-ntfs" (into-array java.nio.file.attribute.FileAttribute [])))

(defn- sh! [& args]
  (let [{:keys [exit out err]} (apply sh/sh args)]
    (when-not (zero? exit)
      (throw (ex-info (str (first args) " scheiterte") {:out out :err err})))
    out))

(def ^:private big-name "gross.bin")
(def ^:private big-size (* 8 1024 1024))

(defn- pattern-file! [^Path p size seed]
  (with-open [os (io/output-stream (.toFile p))]
    (let [buf (byte-array 4096)]
      (dotimes [i (quot size 4096)]
        (dotimes [j 4096] (aset-byte buf j (unchecked-byte (rem (+ (* i 7) j seed) 251))))
        (.write os buf)))))

(defn- mkfs!
  "Ein NTFS-Abbild mit Inhalt. Rueckgabe ist der Pfad."
  [^Path dir ^String img bytes- cluster ^Path src-dir]
  (let [p (str (.resolve dir img))]
    (with-open [raf (java.io.RandomAccessFile. p "rw")] (.setLength raf bytes-))
    (sh! (tool "mkntfs") "-Q" "-F" "-c" (str cluster) "-L" "test" p)
    (doseq [f (sort (.list (.toFile src-dir)))]
      (sh! (tool "ntfscp") p (str (.resolve src-dir ^String f)) (str "/" f)))
    p))

(defn- ntfscluster-free ^long [^String img]
  (->> (str/split-lines (:out (sh/sh (tool "ntfscluster") img)))
       (keep #(second (re-matches #"^clusters of free space\s*:\s*(\d+)$" (str/trim %))))
       first parse-long))

(defn- ntfscluster-at
  "Welche Datei laut ntfs-3g am Cluster n liegt, z.B. \"/gross.bin\"."
  [^String img ^long n]
  (->> (str/split-lines (:out (sh/sh (tool "ntfscluster") "--cluster" (str n) img)))
       (keep #(second (re-find #"Inode \d+ (/\S+?)/\$DATA" %)))
       first))

(defn- reader-over [^String img]
  (let [s (src/file-source (Path/of img (into-array String [])))]
    [s (fn [off len] (let [b (byte-array len)] (src/s-read s off b len) b))]))

(defn- with-image [f]
  (let [dir (tmpdir)
        srcd (.resolve dir "inhalt")]
    (try
      (Files/createDirectories srcd (into-array java.nio.file.attribute.FileAttribute []))
      (pattern-file! (.resolve srcd big-name) big-size 0)
      (pattern-file! (.resolve srcd "zweite.bin") (* 3 1024 1024) 13)
      (spit (.toFile (.resolve srcd "klein.txt")) (str/join (repeat 20 "winzig\n")))
      (f dir srcd)
      (finally
        (doseq [x (reverse (file-seq (.toFile dir)))] (.delete x))))))

(deftest belegung-deckt-sich-mit-ntfscluster
  (if-not (tools? "mkntfs" "ntfscp" "ntfscluster")
    (println "  (kein ntfs-3g auf diesem Host -- NTFS-Gegenprobe uebersprungen)")
    (with-image
      (fn [dir srcd]
        (doseq [[img bytes- cluster]
                [["ntfs-4k.img" (* 192 1024 1024) 4096]
                 ;; 512-Byte-Cluster: der MFT-Satz von 1024 Byte geht dann ueber
                 ["ntfs-512.img" (* 64 1024 1024) 512]]]
          (testing (str cluster "-Byte-Cluster")
            (let [p (mkfs! dir img bytes- cluster srcd)
                  [s read-at] (reader-over p)]
              (try
                (let [bt (ntfs/boot read-at)
                      probe (ntfs/probe read-at {})]
                  (is (some? bt) "der Bootsektor muss als NTFS erkannt werden")
                  (is (= cluster (:cluster-size bt)))
                  (is (some? probe) "ein NTFS-Dateisystem muss gelesen werden koennen")

                  (testing "Stufe 1: belegte Cluster == total - frei laut ntfscluster"
                    (let [total (long (:total-clusters (:info probe)))
                          frei (ntfscluster-free p)
                          gelesen (quot (long (:allocated probe)) cluster)]
                      (is (= (- total frei) gelesen)
                          "jeder belegte Cluster ist dabei, kein freier zu viel")))

                  (testing "das Dateisystem ist genau so gross wie die Datei"
                    (is (= (Files/size (Path/of p (into-array String [])))
                           (long (:length probe)))))

                  (testing "Stufe 2: an jedem Schnitt faengt laut ntfs-3g wirklich eine Datei an"
                    (is (seq (:cuts probe)) "es muss Schnitte geben")
                    (doseq [c (:cuts probe)]
                      (is (some? (ntfscluster-at p (quot (long c) cluster)))
                          (str "Cluster " (quot (long c) cluster) " gehoert zu keiner Datei"))))

                  (testing "die grosse Datei hat einen Schnitt"
                    (is (some (fn [c] (= (str "/" big-name)
                                         (ntfscluster-at p (quot (long c) cluster))))
                              (:cuts probe))
                        (str big-name " muss einen eigenen Schnitt bekommen")))

                  (testing "der freie Teil wird gar nicht erst gelesen"
                    (is (< (long (:allocated probe)) (* 0.4 (long (:length probe))))
                        "ein fast leeres Abbild darf nicht vollstaendig gelesen werden")))
                (finally (src/s-close s))))))))))

(deftest am-schnitt-stehen-die-ersten-bytes-der-datei
  (if-not (tools? "mkntfs" "ntfscp" "ntfscluster")
    (println "  (kein ntfs-3g -- uebersprungen)")
    (with-image
      (fn [dir srcd]
        (let [p (mkfs! dir "lesen.img" (* 192 1024 1024) 4096 srcd)
              [s _] (reader-over p)]
          (try
            (let [es (ntfs/open-partition-source s 0 (src/s-length s) "test" {})
                  cluster 4096
                  at (first (filter (fn [c] (= (str "/" big-name)
                                               (ntfscluster-at p (quot (long c) cluster))))
                                    (src/s-cuts es)))]
              (is (some? at))
              (let [buf (byte-array 4096)
                    soll (byte-array 4096)]
                (src/s-read es at buf 4096)
                (dotimes [j 4096] (aset-byte soll j (unchecked-byte (rem j 251))))
                (is (java.util.Arrays/equals buf soll)
                    "der erste Block der Datei, am errechneten Versatz gelesen")))
            (finally (src/s-close s))))))))

(deftest die-auslagerungsdatei-faellt-heraus
  (if-not (tools? "mkntfs" "ntfscp" "ntfscluster")
    (println "  (kein ntfs-3g -- uebersprungen)")
    (with-image
      (fn [dir srcd]
        (pattern-file! (.resolve srcd "pagefile.sys") (* 6 1024 1024) 7)
        (let [p (mkfs! dir "page.img" (* 192 1024 1024) 4096 srcd)
              [s read-at] (reader-over p)]
          (try
            (let [mit (ntfs/probe read-at {:excludes []})
                  ohne (ntfs/probe read-at {})]
              (is (= 1 (:excluded-files (:info ohne))) "genau eine Datei faellt heraus")
              (is (zero? (long (or (:excluded-files (:info mit)) 0))))
              (is (>= (- (long (:allocated mit)) (src/total (src/normalize (:extents ohne)
                                                                           (:length ohne) 512)))
                      (* 5 1024 1024))
                  "die sechs Megabyte der Auslagerungsdatei stehen nicht mehr im Abbild"))
            (finally (src/s-close s))))))))

(deftest die-quelle-kennt-ihren-hoechsten-belegten-cluster
  (if-not (tools? "mkntfs" "ntfscp") (println "  (kein ntfs-3g -- uebersprungen)")
          (with-image
            (fn [dir srcd]
              (let [p (mkfs! dir "hoch.img" (* 192 1024 1024) 4096 srcd)
                    [s _] (reader-over p)]
                (try
                  (let [es (ntfs/open-partition-source s 0 (src/s-length s) "test" {})
                        info (src/s-info es)
                        high (long (:highest-allocated-byte info))]
                    (is (pos? high))
                    (is (< high (- (src/s-length es) (* 16 1024 1024)))
                        "ein fast leeres Abbild hat viel Luft hinter dem letzten Cluster")
                    ;; Hinter dem letzten belegten Cluster steht nur noch der
                    (is (> (* 64 1024)
                           (src/total (src/subtract (src/s-extents es) [[0 high]]
                                                    (src/s-length es) 512)))
                        "hinter dem letzten belegten Cluster wird fast nichts mehr gelesen"))
                  (finally (src/s-close s))))))))

(deftest ein-zu-kleines-fenster-wird-abgelehnt
  (if-not (tools? "mkntfs") (println "  (kein ntfs-3g -- uebersprungen)")
          (with-image
            (fn [dir srcd]
              (let [p (mkfs! dir "klein.img" (* 64 1024 1024) 4096 srcd)
                    [s _] (reader-over p)]
                (try
                  (is (nil? (ntfs/open-partition-source s 0 (* 32 1024 1024) "zu-klein" {}))
                      "eine Belegungskarte, die ueber das Fenster hinausreicht, gehoert zu etwas anderem")
                  (finally (src/s-close s))))))))

(deftest kein-ntfs-wird-nicht-erkannt
  (let [dir (tmpdir)
        p (str (.resolve dir "leer.img"))]
    (try
      (with-open [raf (java.io.RandomAccessFile. p "rw")] (.setLength raf (* 8 1024 1024)))
      (let [[s _] (reader-over p)]
        (try
          (is (nil? (ntfs/open-partition-source s 0 (src/s-length s) "leer" {}))
              "eine Platte voller Nullen ist kein NTFS")
          (finally (src/s-close s))))
      (finally
        (doseq [f (reverse (file-seq (.toFile dir)))] (.delete f))))))

(defn- fixup-record
  "Ein FILE-Satz von size Bytes mit einer Update-Sequence im Abstand stride: am
   Ende jedes Blocks steht die Pruefzahl, die echten Bytes stehen im Array."
  ^bytes [^long size ^long stride ^long usn echt]
  (let [b (byte-array size)
        cnt (inc (quot size stride))
        put16 (fn [^long at ^long v]
                (aset b (int at) (unchecked-byte (bit-and v 0xFF)))
                (aset b (int (inc at)) (unchecked-byte (bit-and (bit-shift-right v 8) 0xFF))))]
    (System/arraycopy (.getBytes "FILE" "US-ASCII") 0 b 0 4)
    (put16 4 48)            ; usa-offset
    (put16 6 cnt)           ; usa-count
    (put16 48 usn)          ; die Pruefzahl selbst
    (dotimes [i (dec cnt)]
      (put16 (- (* (inc i) stride) 2) usn)                 ; im Satz: die Pruefzahl
      (put16 (+ 48 (* 2 (inc i))) (nth echt i)))           ; im Array: das echte Wort
    b))

(deftest die-update-sequence-folgt-dem-satz-nicht-dem-sektor
  (testing "1024-Byte-Satz, Schrittweite 512"
    (let [b (fixup-record 1024 512 0xBEEF [0x1234 0x5678])]
      (is (true? (ntfs/fixup! b 1024)))
      (is (= 0x1234 (bit-or (bit-and (aget b 510) 0xFF)
                            (bit-shift-left (bit-and (aget b 511) 0xFF) 8))))
      (is (= 0x5678 (bit-or (bit-and (aget b 1022) 0xFF)
                            (bit-shift-left (bit-and (aget b 1023) 0xFF) 8))))))
  (testing "4096-Byte-Satz, Schrittweite immer noch 512 -- nicht die Sektorgroesse"
    (let [b (fixup-record 4096 512 0x0102 (repeat 8 0x4142))]
      (is (true? (ntfs/fixup! b 4096)))
      (is (= 0x42 (bit-and (aget b 4094) 0xFF)) "kleines Byte zuerst")))
  (testing "eine Pruefzahl, die nicht passt, macht den Satz unbrauchbar"
    (let [b (fixup-record 1024 512 0xBEEF [0x1234 0x5678])]
      (aset b 1022 (unchecked-byte 0))
      (is (false? (ntfs/fixup! b 1024))))))

(deftest lauflisten-werden-richtig-dekodiert
  (testing "ein Lauf, zwei Bytes Laenge, ein Byte Versatz"
    (is (= [[0x34 0x0100]]
           (ntfs/decode-runs (byte-array [0x12 0x00 0x01 0x34 0x00]) 0 5))))
  (testing "der Versatz zaehlt ab dem vorigen Lauf und darf negativ sein"
    (is (= [[100 8] [90 4]]
           (ntfs/decode-runs (byte-array [0x11 0x08 100
                                          0x11 0x04 (unchecked-byte -10)
                                          0x00])
                             0 8))))
  (testing "Versatzbreite 0 ist ein duenn besetzter Lauf und gehoert in kein Abbild"
    (is (= [[5 2] [9 2]]
           (ntfs/decode-runs (byte-array [0x11 0x02 0x05
                                          0x01 0x04
                                          0x11 0x02 0x04
                                          0x00])
                             0 9))
        "die vier Cluster in der Mitte stehen nicht auf der Platte"))
  (testing "eine abgeschnittene Liste endet, statt ueber ihr Ende hinauszulesen"
    (is (= [] (ntfs/decode-runs (byte-array [0x42 0x01]) 0 2)))))

;; --- einzelne Dateien aus einem Abbild --------------------------------------

(defn- ntfsls-names
  "Was ntfs-3g in einem Verzeichnis sieht."
  [^String img ^String dir]
  (->> (:out (if (= "/" dir)
               (sh/sh (tool "ntfsls") "-a" img)
               (sh/sh (tool "ntfsls") "-a" "-p" dir img)))
       str/split-lines
       (map str/trim)
       (remove str/blank?)
       set))

(deftest ein-verzeichnis-zeigt-dasselbe-wie-ntfs-3g
  (if-not (tools? "mkntfs" "ntfscp" "ntfsls")
    (println "  (kein ntfs-3g -- uebersprungen)")
    (with-image
      (fn [dir srcd]
        (let [p (mkfs! dir "baum.img" (* 192 1024 1024) 4096 srcd)
              [s read-at] (reader-over p)]
          (try
            (let [vol (ntfs/open read-at)]
              (is (some? vol))
              (let [namen (set (map :name (ntfs/dir-entries vol 5)))]
                (is (every? namen (ntfsls-names p "/"))
                    "jeder Name, den ntfs-3g in der Wurzel sieht, steht auch hier")
                ;; Der Index eines Verzeichnisses traegt einen Eintrag auf sich
                (is (not (contains? namen "."))
                    "der Selbsteintrag ist kein Kind"))
              (testing "ein echtes Unterverzeichnis"
                (let [ex (ntfs/lookup vol "/$Extend")]
                  (is (:dir? ex))
                  (is (= (ntfsls-names p "/$Extend")
                         (set (map :name (ntfs/dir-entries vol (:frn ex)))))))))
            (finally (src/s-close s))))))))

(deftest ein-pfad-findet-seine-datei
  (if-not (tools? "mkntfs" "ntfscp") (println "  (kein ntfs-3g -- uebersprungen)")
    (with-image
      (fn [dir srcd]
        (let [p (mkfs! dir "pfad.img" (* 192 1024 1024) 4096 srcd)
              [s read-at] (reader-over p)]
          (try
            (let [vol (ntfs/open read-at)]
              (is (= 5 (:frn (ntfs/lookup vol "/"))) "die Wurzel ist Satz 5")
              (let [hit (ntfs/lookup vol (str "/" big-name))]
                (is (some? hit))
                (is (= hit (ntfs/lookup vol (str "\\" (str/upper-case big-name))))
                    "Gross-/Kleinschreibung und der Backslash aendern nichts"))
              (is (some? (ntfs/lookup vol "/$Extend/$Quota"))
                  "ein Pfad ueber zwei Stufen")
              (is (nil? (ntfs/lookup vol "/gibtesnicht.txt")))
              (is (nil? (ntfs/lookup vol (str "/" big-name "/darunter")))
                  "unter einer Datei geht es nicht weiter"))
            (finally (src/s-close s))))))))

(deftest der-inhalt-kommt-byteweise-heraus
  (if-not (tools? "mkntfs" "ntfscp") (println "  (kein ntfs-3g -- uebersprungen)")
    (with-image
      (fn [dir srcd]
        (let [p (mkfs! dir "inhalt.img" (* 192 1024 1024) 4096 srcd)
              [s read-at] (reader-over p)]
          (try
            (let [vol (ntfs/open read-at)
                  holen (fn [pfad]
                          (let [bos (java.io.ByteArrayOutputStream.)]
                            (ntfs/read-file! vol (:frn (ntfs/lookup vol pfad)) bos)
                            (.toByteArray bos)))]
              (testing "eine grosse, nicht-residente Datei"
                (is (java.util.Arrays/equals
                     ^bytes (holen (str "/" big-name))
                     ^bytes (Files/readAllBytes (.resolve srcd big-name)))))
              (testing "eine kleine Datei steht im MFT-Satz selbst"
                (let [i (ntfs/file-info vol (:frn (ntfs/lookup vol "/klein.txt")))]
                  (is (:resident? i))
                  (is (java.util.Arrays/equals
                       ^bytes (holen "/klein.txt")
                       ^bytes (Files/readAllBytes (.resolve srcd "klein.txt"))))))
              (testing "ein Verzeichnis hat keinen Inhalt"
                (let [i (ntfs/file-info vol (:frn (ntfs/lookup vol "/$Extend")))]
                  (is (:dir? i))
                  (is (false? (:readable? i)))
                  (is (string? (:why i)) "und sagt, warum"))))
            (finally (src/s-close s))))))))
