(ns borgclj.linux.volume
  "Platten und Dateisysteme unter Linux auflisten.

   Das Gegenstueck zu borgclj.win.volume, aber ohne dessen Tiefe: dort liest
   ein eigener NTFS-Leser Belegungskarten aus dem Dateisystem, hier fragt
   lsblk das ab, was der Kernel ohnehin kennt. Mehr braucht die Oberflaeche
   nicht -- gelesen und geschrieben wird auf Linux ueber den Dateiweg
   (file:/dev/sda), und dass der traegt, haengt an FileChannel: .size()
   liefert bei einem Blockgeraet dessen Groesse, und geoeffnet wird ohne
   Truncate. Deshalb kommt hier nur die Auflistung dazu und sonst nichts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- lsblk
  "lsblk in der Form NAME=\"...\" TYPE=\"...\", eine Zeile je Geraet. Das Paarformat
   ist das einzige, das mit Leerzeichen in Modellnamen und Bezeichnungen sicher
   umgeht."
  []
  (let [cols "NAME,KNAME,TYPE,SIZE,FSTYPE,LABEL,MOUNTPOINT,PKNAME,PARTN,MODEL,PHY-SEC,PTTYPE,FSSIZE,FSAVAIL,TRAN"
        ;; Die Typangabe muss sein: ohne sie waehlt Clojure den Konstruktor
        ;; reflektiv, und im native-image gibt es die Klasse dann nicht --
        ;; der Aufruf scheitert mit "java.lang.ProcessBuilder".
        pb (doto (ProcessBuilder. ^java.util.List (vec ["lsblk" "-Pbo" cols]))
             (.redirectErrorStream false))
        p (.start pb)
        out (with-open [r (io/reader (.getInputStream p))] (vec (line-seq r)))]
    (.waitFor p)
    (when-not (zero? (.exitValue p))
      (throw (ex-info "lsblk ist fehlgeschlagen" {:exit (.exitValue p)})))
    out))

(defn- parse-line [^String line]
  (into {} (for [[_ k v] (re-seq #"([A-Z_-]+)=\"([^\"]*)\"" line)]
             [(keyword (str/lower-case (str/replace k "_" "-")))
              (when (seq v) v)])))

(defn- n-of [m k] (some-> (get m k) not-empty parse-long))

(defn- style-of [pttype]
  (case pttype "gpt" :gpt "dos" :mbr :raw))

(defn- device ^String [m] (str "/dev/" (or (:kname m) (:name m))))

;; Was die Oberflaeche in ein Formular schreibt. Auf Windows sind das "C:" und
;; "disk:0"; hier ist es der Dateiweg, den parse-source als :file erkennt.
(defn- token ^String [m] (str "file:" (device m)))

(defn- rows
  "Everything but optical drives, zram and loop devices. With
   BORGCLJ_LOOP_TARGETS=1 loop devices stay in -- so a recovery can be tried
   against an image file (losetup -fP) without touching a real disk."
  []
  (let [loops? (= "1" (System/getenv "BORGCLJ_LOOP_TARGETS"))]
    (->> (lsblk)
         (map parse-line)
         (map #(if (and loops? (= "loop" (:type %))) (assoc % :type "disk") %))
         (remove #(contains? #{"loop" "rom"} (:type %)))
         (remove #(str/starts-with? (str (:kname %)) "zram")))))

(defn disks
  "Eine Zeile je ganzer Platte, mit ihren Partitionen darunter."
  []
  (let [all (rows)
        parts (filter #(= "part" (:type %)) all)]
    (vec (for [d all :when (= "disk" (:type d))
               :let [nm (or (:kname d) (:name d))]]
           {:disk nm
            :name nm
            :token (token d)
            :device (device d)
            :model (:model d)
            :interface (:tran d)
            :mountpoint (:mountpoint d)
            :length (or (n-of d :size) 0)
            :sector-size (or (n-of d :phy-sec) 512)
            :style (style-of (:pttype d))
            :partitions
            (vec (for [p parts :when (= (:pkname p) nm)]
                   {:number (or (n-of p :partn) 0)
                    :name (or (:kname p) (:name p))
                    :token (token p)
                    :device (device p)
                    :length (or (n-of p :size) 0)
                    :fstype (:fstype p)
                    :mountpoint (:mountpoint p)
                    :label (or (:label p) (:fstype p))}))}))))

(defn volumes
  "Partitionen, die ein Dateisystem tragen. Die Oberflaeche zeigt sie als das,
   was sich einzeln sichern laesst."
  []
  (vec (for [v (rows)
             :when (and (#{"part" "lvm" "crypt" "raid1" "raid5" "raid0"} (:type v))
                        (:fstype v))]
         {:letter (or (:kname v) (:name v))
          :name (or (:kname v) (:name v))
          :token (token v)
          :device (device v)
          :length (or (n-of v :size) 0)
          :sector-size (or (n-of v :phy-sec) 512)
          :fstype (:fstype v)
          :label (:label v)
          :mountpoint (:mountpoint v)
          :ntfs (= "ntfs" (:fstype v))
          ;; Byte statt Cluster: die Oberflaeche rechnet daraus denselben
          ;; Belegungsanteil, und unter Linux gibt es keine Clustergroesse,
          ;; die fuer alle Dateisysteme gelten wuerde.
          :total-bytes (n-of v :fssize)
          :free-bytes (n-of v :fsavail)
          :extents [{:disk (or (:pkname v) "?")}]})))

(defn providers
  "Es gibt unter Linux keine Schattenkopie-Anbieter im Sinne von VSS. Die
   Oberflaeche bekommt eine leere Liste und sagt es dem Benutzer selbst."
  []
  [])
