(ns borgclj.ui.pages
  (:require [borgclj.log :as log]
            [borgclj.ui.config :as config]
            [borgclj.ui.html :as h]
            [borgclj.ui.job :as job]
            [borgclj.ui.repo :as repo]
            [borgclj.linux.volume :as lv]
            [borgclj.win.ffm :as ffm]
            [borgclj.win.volume :as wv]
            [borgclj.win.vss :as vss]
            [clojure.string :as str]))

(defn- why ^String [^Throwable t]
  (or (ex-message t) (.getName (class t))))

(defn- cfg [app] @(:config app))

(defn- linux? []
  (str/starts-with? (str (System/getProperty "os.name")) "Linux"))

(defn disks
  "Die Hardware zu lesen ist auf Windows ein Systemaufruf, auf Linux eine Frage
   an lsblk. Beide liefern dieselbe Form; scheitert es, sagt die Seite es, statt
   zu verschwinden."
  [app force]
  (when (or force (nil? @(:disks app)))
    (reset! (:disks app)
            (try (if (linux?)
                   {:disks (lv/disks) :volumes (lv/volumes) :providers (lv/providers)}
                   {:disks (wv/disks) :volumes (wv/volumes) :providers (vss/probe)})
                 (catch Throwable t {:error (why t)}))))
  @(:disks app))

;; Anzeigename und Formularwert unterscheiden sich je Plattform: "C:" und
;; "disk:0" auf Windows, "/dev/sda1" und "file:/dev/sda1" auf Linux. Die
;; Backends legen :name und :token bei; fehlen sie, gilt die Windows-Form.
(defn- vol-label [v] (or (:name v) (str (:letter v) ":")))
(defn- vol-token [v] (or (:token v) (str (:letter v) ":")))
(defn- disk-label [dk] (or (:name dk) (str "disk:" (:disk dk))))
(defn- disk-token [dk] (or (:token dk) (str "disk:" (:disk dk))))
;; Beim Zurueckspielen ist das Ziel ein Pfad, keine Quellenkennung: --to nimmt
;; /dev/sda, waehrend --source file:/dev/sda erwartet.
(defn- disk-target [dk] (or (:device dk) (str "disk:" (:disk dk))))
(defn- vol-target [v] (or (:device v) (str (:letter v) ":")))
(defn- fs-of [v] (or (:fstype v) (if (:ntfs v) "ntfs" "other")))

(defn- used-fraction [v]
  ;; Windows zaehlt in Clustern, Linux in Byte -- der Anteil ist derselbe.
  (let [total (long (or (:total-bytes v) (:total-clusters v) 0))
        free (long (or (:free-bytes v) (:free-clusters v) 0))]
    (when (pos? total) (/ (double (- total free)) (double total)))))

(defn- warn [msg] (str "<div class=\"warn\">" (h/esc msg) "</div>"))

(defn- busy [app]
  (when (job/running? (:job app))
    (str "<div class=\"flash\">Es läuft bereits ein Auftrag. "
         "<a href=\"/lauf\">Zum laufenden Auftrag</a></div>")))

(defn- row [caption nm value hint]
  (str "<div class=\"row\"><label for=\"" nm "\">" (h/esc caption) "</label>"
       "<input type=\"text\" id=\"" nm "\" name=\"" nm "\" value=\"" (h/esc value) "\">"
       (when hint (str "<div class=\"hint\">" (h/esc hint) "</div>"))
       "</div>"))

(defn- options [values chosen]
  (apply str (for [v values]
               (str "<option" (when (= v chosen) " selected") ">" (h/esc v) "</option>"))))

(defn- check [nm label on?]
  (str "<div class=\"row\"><label><input type=\"checkbox\" name=\"" nm "\" value=\"1\""
       (when on? " checked") "> " label "</label></div>"))

;; ------------------------------------------------------------------ pages ---

(defn overview [app _req]
  (let [d (disks app false)
        body
        (str
         (when (:error d)
           (str "<div class=\"warn\"><b>Die Platten lassen sich nicht auflisten.</b>"
                "<pre class=\"log\">" (h/esc (:error d)) "</pre></div>"))
         "<p><a class=\"btn\" href=\"/uebersicht?neu=1\">Aktualisieren</a></p>"
         (when (seq (:disks d))
           (str "<div class=\"card\"><table><tr><th>Platte / Partition</th><th>Größe</th>"
                "<th>Art</th><th>Beschreibung</th></tr>"
                (apply str
                       (for [dk (:disks d)]
                         (str "<tr><td><b>" (h/esc (disk-label dk)) "</b></td>"
                              "<td class=\"num\">" (h/esc (log/human (long (:length dk)))) "</td>"
                              "<td>" (h/esc (name (:style dk))) "</td><td class=\"muted\">"
                              (count (:partitions dk)) " Partitionen, Sektor "
                              (:sector-size dk) " Byte</td></tr>"
                              (apply str
                                     (for [p (:partitions dk)]
                                       (str "<tr class=\"part\"><td>#" (:number p) "</td>"
                                            "<td class=\"num\">"
                                            (h/esc (log/human (long (:length p)))) "</td>"
                                            "<td></td><td>" (h/esc (or (:label p) "")) "</td></tr>"))))))
                "</table></div>"))
         (when (seq (:volumes d))
           (str "<div class=\"card\"><table><tr><th>Volume</th><th>Größe</th><th>Dateisystem</th>"
                "<th>Belegt</th><th>auf Platte</th></tr>"
                (apply str
                       (for [v (:volumes d)]
                         (str "<tr><td><b>" (h/esc (vol-label v)) "</b></td>"
                              (if (:error v)
                                (str "<td colspan=\"4\" class=\"bad\">nicht lesbar: "
                                     (h/esc (:error v)) "</td>")
                                (let [f (used-fraction v)
                                      len (long (or (:length v) 0))]
                                  (str "<td class=\"num\">" (h/esc (log/human len)) "</td>"
                                       "<td>" (h/esc (fs-of v)) "</td><td>"
                                       (if f
                                         (str (log/fmt "%.0f %%" (* 100.0 f)) " — "
                                              (h/esc (log/human (long (* f len)))) " von "
                                              (h/esc (log/human len)))
                                         "<span class=\"muted\">unbekannt</span>")
                                       "</td><td class=\"muted\">"
                                       (h/esc (str/join "," (map :disk (:extents v)))) "</td>")))
                              "</tr>")))
                "</table></div>"))
         "<div class=\"card\"><b>Schattenkopie-Anbieter (VSS)</b><p>"
         (if (seq (:providers d))
           (str/join " · "
                     (for [p (:providers d)]
                       (if (:available p)
                         (str "<span class=\"ok\">" (h/esc (name (:provider p))) ": verfügbar</span>")
                         (str "<span class=\"muted\">" (h/esc (name (:provider p)))
                              ": nicht verfügbar</span>"))))
           ;; Im Rettungssystem ist das kein Mangel: dort laeuft nichts von der
           ;; Platte, die gesichert wird, also ist das Abbild ohnehin ruhig.
           (if (linux?)
             (str "<span class=\"muted\">unter Linux ohne Belang — hier läuft nichts von "
                  "der Platte, die gesichert wird, das Abbild ist also ruhig</span>")
             (str "<span class=\"muted\">keiner gemeldet — ohne Schattenkopie ist ein Abbild "
                  "bestenfalls absturzkonsistent</span>")))
         "</p></div>")]
    (h/layout {:active "/uebersicht" :title "Übersicht" :head ""
               :subtitle (if (linux?)
                           "Platten und Dateisysteme dieses Rechners."
                           "Platten, Volumes und Schattenkopie-Anbieter dieses Rechners.")
               :body body})))

(defn target
  ([app req] (target app req nil))
  ([app _req flash]
   (let [c (cfg app)
         body
         (str
          (when flash (str "<div class=\"flash\">" (h/esc flash) "</div>"))
          "<form method=\"post\" action=\"/ziel\"><div class=\"card\">"
          (row "Repository" "repo" (:repo c) "z. B. ssh://backup@nas/srv/borg/repo")
          (row "SSH-Schlüssel" "identity" (:identity c) "--identity")
          (row "Benutzer" "user" (:user c) "--user, falls der Ort keinen nennt")
          (row "borg auf der Gegenseite" "remote-path" (:remote-path c) "--remote-path")
          (row "eigener Aufruf statt ssh" "rsh" (:rsh c) "--rsh")
          (row "Server-Befehl statt ssh" "server-cmd" (:server-cmd c)
               "--server-cmd, z. B. \"borg serve\" für ein Repository auf einer lokalen Platte")
          "</div><div class=\"card\">"
          "<div class=\"row\"><label for=\"compression\">Kompression</label>"
          "<select id=\"compression\" name=\"compression\">"
          (options ["none" "lz4" "zstd,1" "zstd,3" "zstd,9"] (:compression c))
          "</select></div>"
          (row "Archivname" "archive" (:archive c)
               (str "{hostname} und {date} werden ersetzt — Vorgabe ergibt "
                    (config/archive-name (:archive c))))
          (row "Blockgröße" "block" (:block c) "--block, Vorgabe 4M")
          "<div class=\"row\"><label for=\"snapshot\">Schattenkopie</label>"
          "<select id=\"snapshot\" name=\"snapshot\">"
          (options ["auto" "vshadow" "wmi" "vssadmin" "none"] (:snapshot c))
          "</select></div>"
          (row "zusätzlich auslassen" "exclude" (:exclude c)
               "volumenrelative Pfade, mit Komma getrennt")
          (check "init" "Repository anlegen, falls es noch nicht besteht (--init)" (:init c))
          "</div>"
          "<p><button class=\"primary\" type=\"submit\" name=\"tun\" value=\"speichern\">Speichern</button> "
          "<button type=\"submit\" name=\"tun\" value=\"pruefen\">Speichern und Verbindung prüfen</button></p>"
          "</form>")]
     (h/layout {:active "/ziel" :title "Ziel einrichten" :head "" :body body
                :subtitle "Wohin gesichert wird, und womit sich der Rechner dort anmeldet."}))))

(defn post-target [app req]
  (let [p (:param req)
        c (into (cfg app)
                (for [k [:repo :identity :user :remote-path :rsh :server-cmd
                         :compression :archive :block :snapshot :exclude]]
                  [k (str/trim (p (name k)))]))
        c (assoc c :init (= "1" (p "init")))]
    (reset! (:config app) c)
    (config/save! c)
    (target app req
            (if (= "pruefen" (p "tun"))
              (try (str "Gespeichert. Repository erreichbar — "
                        (count (repo/archive-names c)) " Sicherungen.")
                   (catch Throwable t
                     (str "Gespeichert, aber das Repository antwortet nicht: " (why t))))
              (str "Gespeichert in " (config/config-path))))))

(defn backup
  ([app req] (backup app req nil))
  ([app _req flash]
   (let [c (cfg app)
         d (disks app false)
         body
         (str
          (when flash (warn flash))
          (busy app)
          "<form method=\"post\" action=\"/sichern\"><div class=\"card\"><b>Quellen</b>"
          (if (:error d)
            (str "<p class=\"muted\">Die Liste ist leer, weil sich die Platten hier nicht "
                 "auflisten lassen. Quellen lassen sich trotzdem von Hand eintragen.</p>")
            (str "<ul class=\"srcs\">"
                 (apply str
                        (for [v (:volumes d) :when (not (:error v))]
                          (str "<li><label><input type=\"checkbox\" name=\"src\" value=\""
                               (h/esc (vol-token v)) "\"><span><b>"
                               (h/esc (vol-label v)) "</b> "
                               (h/esc (log/human (long (or (:length v) 0)))) " · "
                               (h/esc (fs-of v)) "</span></label></li>")))
                 (apply str
                        (for [dk (:disks d)]
                          (str "<li><label><input type=\"checkbox\" name=\"src\" value=\""
                               (h/esc (disk-token dk)) "\"><span><b>" (h/esc (disk-label dk)) "</b> "
                               (h/esc (log/human (long (:length dk)))) " · "
                               (h/esc (name (:style dk)))
                               ", ganze Platte</span></label></li>")))
                 "</ul>"))
          "<div class=\"row\" style=\"margin-top:14px\"><label for=\"extra\">weitere Quellen von Hand</label>"
          "<input type=\"text\" id=\"extra\" name=\"extra\" placeholder=\""
          (if (linux?) "file:/dev/sda1, file:/dev/sda" "C:, disk:0, file:/pfad") "\">"
          "<div class=\"hint\">mit Komma getrennt</div></div></div>"
          "<div class=\"card\">"
          (row "Archivname" "archive" (config/archive-name (:archive c)) nil)
          (check "dry-run" (str "Probelauf — öffnet die Quellen, zeigt was gelesen würde, "
                                "rührt das Repository nicht an") true)
          (check "raw" "jeden Sektor lesen, Belegungsbitmap übergehen (--raw)" false)
          (check "no-default-excludes" "pagefile.sys, hiberfil.sys und Papierkorb mitsichern" false)
          "</div>"
          "<p><button class=\"primary\" type=\"submit\">Starten</button></p></form>")]
     (h/layout {:active "/sichern" :title "Sichern" :head "" :body body
                :subtitle "Nur belegte Blöcke werden gelesen; freier Platz wird als Nullen abgelegt."}))))

(defn- start! [app kind title argv]
  (job/start! (:job app)
              {:run (:run app) :kind kind :title title :argv argv
               :stop (when (= :image kind)
                       (.resolve (java.nio.file.Files/createTempDirectory
                                  "borgi-ui" (make-array java.nio.file.attribute.FileAttribute 0))
                                 "stop"))})
  {:redirect "/lauf"})

(defn post-backup [app req]
  (let [c (cfg app)
        p (:param req)
        sources (into [] (comp (map str/trim) (remove str/blank?))
                      (concat ((:params req) "src") (str/split (p "extra") #",")))
        archive (str/trim (p "archive"))
        dry-run (= "1" (p "dry-run"))]
    (try
      (start! app :image
              (str (if dry-run "Probelauf " "Sicherung ") archive)
              (config/image-args c {:archive archive :sources sources :dry-run dry-run
                                    :raw (= "1" (p "raw"))
                                    :no-default-excludes (= "1" (p "no-default-excludes"))}))
      (catch Throwable t (backup app req (why t))))))

(defn archives [app req]
  (let [c (cfg app)
        want ((:param req) "a")
        names (try (repo/archive-names c) (catch Throwable t {:error (why t)}))
        body
        (str
         (if (map? names)
           (str "<div class=\"warn\"><b>Das Repository lässt sich nicht lesen.</b>"
                "<pre class=\"log\">" (h/esc (:error names)) "</pre></div>")
           (str "<div class=\"card\"><table><tr><th>Sicherung</th><th></th></tr>"
                (if (empty? names)
                  "<tr><td class=\"muted\">noch keine Sicherung im Repository</td><td></td></tr>"
                  (apply str
                         (for [nm (reverse names)]
                           (str "<tr><td><a href=\"/sicherungen?a=" (h/url-enc nm) "\">"
                                (h/esc nm) "</a></td><td><a class=\"btn\" href=\"/zurueck?a="
                                (h/url-enc nm) "\">Zurückspielen …</a></td></tr>"))))
                "</table></div>"))
         (when (and (seq want) (not (map? names)))
           (let [loc (config/location c want)
                 conn (config/connection-args c)
                 run (:capture app)]
             (str "<div class=\"card\"><b>" (h/esc want) "</b>"
                  "<p class=\"muted\">borgi info</p><pre class=\"log\">"
                  (h/esc (run (into ["info" loc] conn))) "</pre>"
                  "<p class=\"muted\" style=\"margin-top:14px\">borgi plan</p>"
                  "<pre class=\"log\">" (h/esc (run (into ["plan" loc] conn))) "</pre></div>"))))]
    (h/layout {:active "/sicherungen" :title "Sicherungen" :head "" :body body
               :subtitle "Was im Repository liegt."})))

(defn- parent-path ^String [^String p]
  (let [i (.lastIndexOf p "/")]
    (if (pos? i) (subs p 0 i) "/")))

(defn files
  ([app req] (files app req nil))
  ([app req err]
   (let [c (cfg app)
         q (:param req)
         names (try (repo/archive-names c) (catch Throwable _ []))
         archive (let [a (str/trim (q "a"))] (if (seq a) a (last names)))
         path (let [p (q "p")] (if (str/blank? p) "/" p))
         image (str/trim (q "i"))
         partition (str/trim (q "n"))
         dest (let [d (str/trim (q "d"))]
                (if (seq d) d (if (ffm/windows?) "C:\\wieder" "/mnt")))
         base (str "/dateien?a=" (h/url-enc archive) "&i=" (h/url-enc image)
                   "&n=" (h/url-enc partition) "&d=" (h/url-enc dest))
         hidden (apply str (for [[k v] [["archive" archive] ["image" image]
                                        ["partition" partition] ["dest" dest]]]
                             (str "<input type=\"hidden\" name=\"" k "\" value=\""
                                  (h/esc v) "\">")))
         listing (when (seq archive)
                   (try (repo/entries c {:archive archive :image image
                                         :partition partition :path path})
                        (catch Throwable t {:error (why t)})))
         body
         (str
          (when err (warn err))
          (busy app)
          "<form method=\"get\" action=\"/dateien\"><div class=\"card\">"
          "<div class=\"row\"><label for=\"a\">Sicherung</label>"
          (if (seq names)
            (str "<select id=\"a\" name=\"a\">" (options (reverse names) archive) "</select>")
            (str "<input type=\"text\" id=\"a\" name=\"a\" value=\"" (h/esc archive) "\">"))
          "</div>"
          (row "Partition" "n" partition
               "leer lassen: die erste Partition mit einem lesbaren Dateisystem")
          (row "Abbild im Archiv" "i" image "leer lassen, wenn das Archiv nur eines enthält")
          (row "Zielordner auf diesem Rechner" "d" dest nil)
          "<p><button type=\"submit\">Öffnen</button></p></div></form>"
          (cond
            (str/blank? archive) nil

            (:error listing)
            (str "<div class=\"warn\"><b>" (h/esc path) " lässt sich nicht lesen.</b>"
                 "<pre class=\"log\">" (h/esc (:error listing)) "</pre></div>")

            :else
            (str "<div class=\"card\"><p class=\"muted\">" (h/esc archive) "  ·  "
                 (h/esc path) "</p><table><tr><th>Name</th><th>Größe</th><th></th></tr>"
                 (when (not= "/" path)
                   (str "<tr><td><a href=\"" base "&p=" (h/url-enc (parent-path path))
                        "\">..</a></td><td></td><td></td></tr>"))
                 (if (empty? listing)
                   "<tr><td class=\"muted\">leer</td><td></td><td></td></tr>"
                   (apply str
                          (for [e listing
                                :let [child (if (= "/" path) (str "/" (:name e))
                                                (str path "/" (:name e)))]]
                            (str "<tr><td>"
                                 (if (:dir? e)
                                   (str "<a href=\"" base "&p=" (h/url-enc child) "\">"
                                        (h/esc (:name e)) "/</a>")
                                   (h/esc (:name e)))
                                 "</td><td>" (h/esc (or (:size e) "")) "</td><td>"
                                 "<form method=\"post\" action=\"/dateien\">" hidden
                                 "<input type=\"hidden\" name=\"path\" value=\"" (h/esc child) "\">"
                                 "<button type=\"submit\">Holen</button></form></td></tr>"))))
                 "</table></div>"
                 "<form method=\"post\" action=\"/dateien\"><div class=\"card\">" hidden
                 "<input type=\"hidden\" name=\"path\" value=\"" (h/esc path) "\">"
                 "<p>Alles unter <b>" (h/esc path) "</b> nach <b>" (h/esc dest) "</b> holen.</p>"
                 "<p class=\"hint\">Was NTFS selbst komprimiert oder mit EFS verschlüsselt hat, "
                 "wird übersprungen und im Protokoll benannt — der Rest kommt trotzdem an.</p>"
                 "<p><button type=\"submit\">Ganzen Ordner holen</button></p></div></form>")))]
     (h/layout {:active "/dateien" :title "Dateien holen" :head "" :body body
                :subtitle "Einzelne Dateien aus einem Abbild, ohne es auszupacken."}))))

(defn post-fetch [app req]
  (let [p (:param req)]
    (try
      (start! app :fetch (str "Holen " (p "path") " nach " (p "dest"))
              (config/fetch-args (cfg app) {:archive (p "archive") :path (p "path")
                                            :dest (p "dest") :image (p "image")
                                            :partition (p "partition")}))
      (catch Throwable t (files app req (why t))))))

(defn restore
  ([app req] (restore app req nil))
  ([app req err]
   (let [c (cfg app)
         d (disks app false)
         p (:param req)
         archive (let [a (p "a")] (if (seq a) a (p "archive")))
         names (try (repo/archive-names c) (catch Throwable _ []))
         body
         (str
          "<div class=\"warn\"><b>Das Ziel wird vollständig überschrieben.</b> "
          "Alles, was dort steht, ist danach weg. Für diesen Schritt gibt es keinen Probelauf "
          "und kein Zurück.</div>"
          (when err (warn err))
          (busy app)
          "<form method=\"post\" action=\"/zurueck\"><div class=\"card\">"
          "<div class=\"row\"><label for=\"archive\">Archiv</label>"
          (if (seq names)
            (str "<select id=\"archive\" name=\"archive\">"
                 (options (reverse names) archive) "</select>")
            (str "<input type=\"text\" id=\"archive\" name=\"archive\" value=\""
                 (h/esc archive) "\">"))
          "</div>"
          "<div class=\"row\"><label for=\"target\">Ziel</label>"
          "<input type=\"text\" id=\"target\" name=\"target\" list=\"ziele\" "
          "placeholder=\"" (if (linux?) "/dev/sda oder ein Dateipfad" "D:, disk:1, \\\\.\\PhysicalDrive1 oder ein Dateipfad") "\">"
          "<datalist id=\"ziele\">"
          (apply str (for [dk (:disks d)] (str "<option value=\"" (h/esc (disk-target dk)) "\">")))
          (apply str (for [v (:volumes d) :when (not (:error v))]
                       (str "<option value=\"" (h/esc (vol-target v)) "\">")))
          "</datalist></div>"
          (row "Abbild im Archiv" "image" "" "leer lassen, wenn das Archiv nur eines enthält")
          (row "nur diese Partition" "partition" "" nil)
          (check "to-file" "Ziel ist eine Datei (--to-file)" false)
          (check "vhd" "VHD-Fußzeile anhängen" false)
          (check "skip-zeros" "Nullblöcke nicht schreiben" false)
          (check "force" "auch die laufende Systemplatte zulassen (--force)" false)
          "</div><div class=\"card\">"
          (row "Zum Bestätigen das Ziel noch einmal eintippen" "bestaetigung" ""
               (str "Muss Zeichen für Zeichen mit dem Ziel oben übereinstimmen. Ein Knopf "
                    "allein ist zu leicht zu treffen für etwas, das eine Platte löscht."))
          "</div><p><button class=\"danger\" type=\"submit\">Zurückspielen</button></p></form>")]
     (h/layout {:active "/zurueck" :title "Zurückspielen" :head "" :body body
                :subtitle "Schreibt ein Abbild auf eine Platte, ein Volume oder in eine Datei."}))))

(defn post-restore [app req]
  (let [p (:param req)
        target (str/trim (p "target"))]
    ;; the typed confirmation is the last guard before a disk is overwritten
    (if (or (str/blank? target) (not= target (str/trim (p "bestaetigung"))))
      (restore app req (str "Nicht gestartet: die Bestätigung stimmt nicht Zeichen für "
                            "Zeichen mit dem Ziel überein."))
      (try
        (start! app :restore
                (str "Zurückspielen " (p "archive") " nach " target)
                (config/restore-args (cfg app)
                                     {:archive (p "archive") :target target
                                      :image (p "image") :partition (p "partition")
                                      :to-file (= "1" (p "to-file")) :vhd (= "1" (p "vhd"))
                                      :skip-zeros (= "1" (p "skip-zeros"))
                                      :force (= "1" (p "force"))}))
        (catch Throwable t (restore app req (why t)))))))

;; -------------------------------------------------------------------- run ---

(defn percent-of [s]
  (let [p (:progress s)]
    (cond
      (= :done (:state s)) 100.0
      ;; the last line reported can sit just short of the end; a finished run is
      ;; finished
      (:percent p) (double (:percent p))
      :else nil)))

(defn numbers-of ^String [s]
  (let [{:keys [phase] :as p} (:progress s)]
    (case phase
      :copy (str/join " · "
                      [(str (:source p))
                       (str (log/human (long (:read p 0))) " gelesen")
                       (str (log/human (long (:zeroed p 0))) " als Nullen")
                       (str (log/human (long (:sent p 0))) " neu übertragen")
                       (h/rate (:read-per-second p))])
      :restore (str (log/human (long (:done p 0))) " von "
                    (log/human (long (:of p 0))) " geschrieben · "
                    (h/rate (:per-second p)))
      :mft-records (str "MFT gelesen: " (:records p) " Sätze")
      :mft-dirs (str "MFT: " (:dirs p) " von " (:dirs-total p) " Verzeichnissen, "
                     (:files p) " Dateien")
      :start "Beginnt …"
      :snapshot (str "Schattenkopie: " (name (:provider p)))
      :scanning "Quellen werden geöffnet …"
      :connected "Mit dem Repository verbunden."
      :done (str "Abbild " (log/human (long (:size p 0))) ", "
                 (log/human (long (:read p 0))) " gelesen, "
                 (log/human (long (:sent p 0))) " neu übertragen")
      :stopped (str "Angehalten bei Block " (:block p) " von " (:of p))
      :failed (:error p)
      nil)))

(defn- eta-text ^String [s]
  (if-let [e (and (= :running (:state s)) (:eta-seconds (:progress s)))]
    (str "noch etwa " (h/duration e))
    ""))

(defn run-page [app _req]
  (let [s (job/snapshot (:job app))
        pct (percent-of s)
        numbers (numbers-of s)
        [lines _] (job/log-from s 0)
        ;; Without JavaScript the page refreshes itself; with it, app.js takes
        ;; over the same numbers. Both only while a job runs.
        head (when (= :running (:state s))
               (str "<noscript><meta http-equiv=\"refresh\" content=\"2\"></noscript>"
                    "<script src=\"/app.js\" defer></script>"))
        body
        (str
         "<div class=\"card\" id=\"lauf\">"
         (when (:title s) (str "<p class=\"muted\">" (h/esc (:title s)) "</p>"))
         "<span class=\"pct\">" (if pct (h/percent pct) "—") "</span>"
         "<span class=\"eta\">" (h/esc (eta-text s)) "</span>"
         "<div class=\"bar\"><i style=\"width:" (log/fmt "%.1f" (or pct 0.0)) "%\"></i></div>"
         "<p class=\"zahlen\">"
         (if numbers (h/esc numbers) "<span class=\"muted\">noch keine Meldung</span>")
         "</p>"
         (case (:state s)
           :running (if (job/stoppable? s)
                      (str "<form method=\"post\" action=\"/abbrechen\">"
                           "<button class=\"danger\" type=\"submit\">Abbrechen</button></form>")
                      ;; only `image` can be told to stop; the others would have
                      ;; to be killed, and killing this process kills the page
                      "<p class=\"muted\">Dieser Auftrag lässt sich nicht abbrechen.</p>")
           :done "<p class=\"ok\"><b>Fertig.</b></p>"
           :cancelled "<p class=\"muted\"><b>Abgebrochen.</b></p>"
           :failed "<p class=\"bad\"><b>Fehlgeschlagen.</b></p>"
           :idle (str "<p class=\"muted\">Bisher lief nichts. "
                      "<a href=\"/sichern\">Eine Sicherung starten</a></p>"))
         "<p class=\"muted\" style=\"margin-top:16px\">Protokoll</p>"
         "<pre class=\"log\">" (apply str (map #(str (h/esc %) "\n") lines)) "</pre></div>")]
    (h/layout {:active "" :title "Auftrag" :head head :body body
               :subtitle "Der laufende oder zuletzt gelaufene Auftrag."})))

(defn event-payload
  "One `zustand` event for app.js: the same numbers the run page renders."
  [s lines]
  (h/json {:state (name (:state s))
           :hasProgress (some? (percent-of s))
           :percent (or (percent-of s) 0.0)
           :zahlen (or (numbers-of s) "")
           :eta (eta-text s)
           :lines (vec lines)}))
