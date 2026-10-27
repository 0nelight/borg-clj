(ns borgclj.gui.i18n
  "Every text the window shows, in English and German.

   English follows the wording of the Acronis True Image rescue media the
   workflow is modelled on (\"Recovery Wizard\", \"Archive selection\", \"What to
   recover\"), German that of its German edition.

   The language is picked once at start: BORGCLJ_LANG=en|de, else borgclj.lang=
   on the kernel command line (the recovery system), else the system locale --
   German for a German locale, English for everything else. The JDK's own
   dialogs (OK/Cancel, the folder chooser) follow the same choice, because the
   default locale is set to it before the first window opens."
  (:require [clojure.string :as str])
  (:import (java.util Locale)))

(set! *warn-on-reflection* true)

(def strings
  {;; --- main window
   :app/welcome {:en "Welcome to borg-imager" :de "Willkommen bei borg-imager"}
   :app/what {:en "What would you like to do?" :de "Was möchten Sie tun?"}
   :app/backup {:en "Back Up" :de "Sichern"}
   :app/recover {:en "Recover" :de "Wiederherstellen"}
   :app/my-disks {:en "My Disks" :de "Meine Laufwerke"}
   :app/files {:en "Files & Folders" :de "Dateien & Ordner"}
   :app/letters {:en "Local volume letters and names may be different from those in Windows."
                 :de "Laufwerksbuchstaben und Namen können von denen unter Windows abweichen."}
   :nav/home {:en "Home" :de "Startseite"}
   :nav/backup {:en "Backup" :de "Sichern"}
   :nav/recovery {:en "Recovery" :de "Wiederherstellung"}
   :nav/log {:en "Log" :de "Protokoll"}
   :nav/tools {:en "Tools & Utilities" :de "Extras & Werkzeuge"}
   :nav/disks {:en "Disks" :de "Laufwerke"}
   :sec/backup-disks {:en "Back up whole disks or partitions as an image"
                      :de "Ganze Platten oder Partitionen als Abbild sichern"}
   :sec/backup-files {:en "Back up selected files and folders"
                      :de "Ausgewählte Dateien und Ordner sichern"}
   :sec/recover-disks {:en "Recover disks or partitions from a backup"
                       :de "Platten oder Partitionen aus einer Sicherung zurückspielen"}
   :sec/recover-files {:en "Get single files and folders out of an image"
                       :de "Einzelne Dateien und Ordner aus einem Abbild holen"}
   :menu/repository {:en "Repository settings …" :de "Repository einstellen …"}
   :menu/disks {:en "Show disks" :de "Laufwerke anzeigen"}
   ;; --- log
   :log/empty {:en "No operation has run in this session yet."
               :de "In dieser Sitzung ist noch kein Vorgang gelaufen."}
   :log/operations {:en "Operations" :de "Vorgänge"}
   :log/time {:en "Time" :de "Zeit"}
   :log/operation {:en "Operation" :de "Vorgang"}
   :log/result {:en "Result" :de "Ergebnis"}
   :log/done {:en "Succeeded" :de "Erfolgreich"}
   :log/failed {:en "Failed" :de "Fehlgeschlagen"}
   :log/cancelled {:en "Cancelled" :de "Abgebrochen"}
   ;; --- tools
   :tools/repository {:en "Repository" :de "Repository"}
   :tools/repository-link {:en "Set location and login …" :de "Speicherort und Anmeldung einstellen …"}
   :tools/disks-link {:en "Show the disks and partitions of this computer"
                      :de "Laufwerke und Partitionen dieses Rechners anzeigen"}
   :tools/verify {:en "Validate backups" :de "Sicherungen prüfen"}
   :tools/verify-link {:en "Read and check every object in the repository (verify)"
                       :de "Jedes Objekt im Repository lesen und prüfen (verify)"}
   :tools/verify-title {:en "Validation" :de "Prüfung"}
   :tools/verify-op {:en "Validate repository" :de "Repository prüfen"}
   :tools/verify-ok {:en "The repository is in order." :de "Das Repository ist in Ordnung."}
   :disks/this {:en "Disks of this computer" :de "Laufwerke dieses Rechners"}
   :disks/col-disk {:en "Disk / partition" :de "Laufwerk / Partition"}
   :disks/col-model {:en "Model / label" :de "Modell / Bezeichnung"}
   :disks/col-kind {:en "Scheme" :de "Art"}
   ;; --- common
   :c/disk-n {:en "Disk %d" :de "Laufwerk %d"}
   :c/capacity {:en "Capacity" :de "Kapazität"}
   :c/used {:en "Used Space" :de "Belegter Platz"}
   :c/type {:en "Type" :de "Typ"}
   :c/flags {:en "Flags" :de "Flags"}
   :c/partition {:en "Partition" :de "Partition"}
   :c/model {:en "Model" :de "Modell"}
   :c/interface {:en "Interface" :de "Schnittstelle"}
   :c/drive {:en "Drive" :de "Laufwerk"}
   :c/volume {:en "Volume" :de "Volume"}
   :c/file-system {:en "File system" :de "Dateisystem"}
   :c/label {:en "Label" :de "Bezeichnung"}
   :c/mounted {:en "Mounted" :de "Eingehängt"}
   :c/browse {:en "Browse …" :de "Durchsuchen …"}
   :c/path {:en "Path:" :de "Pfad:"}
   :c/loading {:en "Loading …" :de "Wird geladen …"}
   :c/failed {:en "That did not work." :de "Das hat nicht geklappt."}
   :c/unknown {:en "unknown" :de "unbekannt"}
   :c/error {:en "Error: %s" :de "Fehler: %s"}
   :c/files-folders {:en "Files and folders" :de "Dateien und Ordner"}
   :c/backup {:en "Backup" :de "Sicherung"}
   :c/primary {:en "Primary // Logical // Dynamic" :de "Primär // Logisch // Dynamisch"}
   :c/unallocated {:en "Unallocated // Unsupported" :de "Nicht zugeordnet // Nicht unterstützt"}
   :c/unallocated-short {:en "Unallocated" :de "Nicht zugeordnet"}
   :c/pri {:en "Pri" :de "Pri"}
   :c/log {:en "Log." :de "Log."}
   :c/act {:en "Act." :de "Akt."}
   ;; --- wizard frame
   :wiz/required {:en "Required steps:" :de "Erforderliche Schritte:"}
   :wiz/optional {:en "Optional steps:" :de "Optionale Schritte:"}
   :wiz/next {:en "Next >" :de "Weiter >"}
   :wiz/cancel {:en "Cancel" :de "Abbrechen"}
   :wiz/proceed {:en "Proceed" :de "Fortfahren"}
   :wiz/options {:en "Options" :de "Optionen"}
   :wiz/help {:en "Help – %s" :de "Hilfe – %s"}
   :wiz/help-default {:en "Work through the steps on the left from top to bottom."
                      :de "Folgen Sie den Schritten links von oben nach unten."}
   :wiz/finish {:en "Finish" :de "Fertig stellen"}
   :wiz/summary {:en "Summary" :de "Zusammenfassung"}
   :wiz/operations {:en "Operations" :de "Vorgänge"}
   :wiz/op-count {:en "Number of operations: %d" :de "Anzahl der Vorgänge: %d"}
   ;; --- progress
   :prog/title {:en "Progress" :de "Fortschritt"}
   :prog/step {:en "Operation %d of %d: %s" :de "Vorgang %d von %d: %s"}
   :prog/current {:en "Current operation: %s" :de "Aktueller Vorgang: %s"}
   :prog/preparing {:en "preparing …" :de "wird vorbereitet …"}
   :prog/total-label {:en "Total progress:" :de "Gesamtfortschritt:"}
   :prog/total {:en "Total %.0f %%" :de "Gesamt %.0f %%"}
   :prog/left {:en "Time left: %s" :de "Verbleibende Zeit: %s"}
   :prog/shutdown {:en "Shut down the computer when the operation completes"
                   :de "Computer nach Abschluss des Vorgangs herunterfahren"}
   :prog/ok {:en "The operation completed successfully." :de "Der Vorgang wurde erfolgreich abgeschlossen."}
   :prog/cancelled {:en "The operation was cancelled." :de "Der Vorgang wurde abgebrochen."}
   :prog/failed {:en "The operation failed: %s" :de "Der Vorgang ist fehlgeschlagen: %s"}
   :pn/read {:en "%s read" :de "%s gelesen"}
   :pn/zeroed {:en "%s as zeros" :de "%s als Nullen"}
   :pn/sent {:en "%s newly sent" :de "%s neu übertragen"}
   :pn/written {:en "%s of %s written" :de "%s von %s geschrieben"}
   :pn/mft-records {:en "MFT read: %s records" :de "MFT gelesen: %s Sätze"}
   :pn/mft-dirs {:en "MFT: %s of %s directories, %s files" :de "MFT: %s von %s Verzeichnissen, %s Dateien"}
   :pn/start {:en "Starting …" :de "Beginnt …"}
   :pn/snapshot {:en "Shadow copy: %s" :de "Schattenkopie: %s"}
   :pn/scanning {:en "Opening sources …" :de "Quellen werden geöffnet …"}
   :pn/connected {:en "Connected to the repository." :de "Mit dem Repository verbunden."}
   :pn/done {:en "Image %s, %s read, %s newly sent" :de "Abbild %s, %s gelesen, %s neu übertragen"}
   :pn/stopped {:en "Stopped at block %s of %s" :de "Angehalten bei Block %s von %s"}
   ;; --- repository dialog
   :conn/title {:en "Select repository" :de "Repository wählen"}
   :conn/heading {:en "Where are the backups?" :de "Wo liegen die Sicherungen?"}
   :conn/repo {:en "Repository" :de "Repository"}
   :conn/repo-hint {:en "e.g. ssh://backup@nas/srv/borg/repo or /mnt/usb/repo"
                    :de "z. B. ssh://backup@nas/srv/borg/repo oder /mnt/usb/repo"}
   :conn/identity {:en "SSH key" :de "SSH-Schlüssel"}
   :conn/identity-hint {:en "path to the private key (--identity)" :de "Pfad zum privaten Schlüssel (--identity)"}
   :conn/user {:en "User" :de "Benutzer"}
   :conn/user-hint {:en "if the location names none (--user)" :de "falls der Ort keinen nennt (--user)"}
   :conn/remote-path {:en "borg on the server" :de "borg auf der Gegenseite"}
   :conn/rsh {:en "Own command instead of ssh" :de "Eigener Aufruf statt ssh"}
   :conn/server-cmd {:en "Server command instead of ssh" :de "Server-Befehl statt ssh"}
   :conn/server-cmd-hint {:en "e.g. borg serve for a repository on a local disk"
                          :de "z. B. borg serve für ein Repository auf einer lokalen Platte"}
   :conn/test {:en "Test connection" :de "Verbindung prüfen"}
   :conn/testing {:en "Testing the connection …" :de "Verbindung wird geprüft …"}
   :conn/reachable {:en "Reachable – %d backups." :de "Erreichbar – %d Sicherungen."}
   :conn/unreachable {:en "Not reachable: %s" :de "Nicht erreichbar: %s"}
   :conn/not-saved {:en "Not saved: %s" :de "Nicht gespeichert: %s"}
   ;; --- recovery wizard
   :rec/title {:en "Recovery Wizard" :de "Wiederherstellungsassistent"}
   :rec/archive {:en "Archive selection" :de "Archivauswahl"}
   :rec/method {:en "Recovery method" :de "Wiederherstellungsmethode"}
   :rec/point {:en "Recovery point" :de "Wiederherstellungspunkt"}
   :rec/what {:en "What to recover" :de "Wiederherzustellende Elemente"}
   :rec/dest-disk {:en "Destination of Disk %d" :de "Ziel von Laufwerk %d"}
   :rec/part-settings {:en "Settings of partition %s" :de "Einstellungen von Partition %s"}
   :rec/dest-head {:en "Destination of MBR and Track 0" :de "Ziel für MBR und Spur 0"}
   :rec/dest {:en "Destination" :de "Ziel"}
   :rec/options {:en "Recovery options" :de "Wiederherstellungsoptionen"}
   :rec/archive-head {:en "Select a backup from which to recover." :de "Wählen Sie die Sicherung, aus der wiederhergestellt wird."}
   :rec/details {:en "Details" :de "Details"}
   :rec/col-name {:en "Name" :de "Name"}
   :rec/col-created {:en "Created" :de "Erstellt"}
   :rec/col-comments {:en "Comments" :de "Kommentar"}
   :rec/col-method {:en "Method" :de "Methode"}
   :rec/col-path {:en "Path" :de "Pfad"}
   :rec/full {:en "Full backup" :de "Vollständig"}
   :rec/empty-repo {:en "There is no backup in this repository yet." :de "In diesem Repository liegt noch keine Sicherung."}
   :rec/method-head {:en "Choose recovery method." :de "Wählen Sie die Wiederherstellungsmethode."}
   :rec/method-disks {:en "Recover whole disks and partitions" :de "Ganze Laufwerke und Partitionen wiederherstellen"}
   :rec/method-files {:en "Recover chosen files and folders" :de "Ausgewählte Dateien und Ordner wiederherstellen"}
   :rec/method-files-hint {:en "Select files and folders to recover from the original disk backup."
                           :de "Wählen Sie Dateien und Ordner aus, die aus der Laufwerkssicherung wiederhergestellt werden sollen."}
   :rec/point-head {:en "Recover your system to a previous point in time." :de "Stellen Sie Ihr System auf einen früheren Zeitpunkt wieder her."}
   :rec/point-text {:en "Select date and time to recover your system to. Recovery may take several minutes."
                    :de "Wählen Sie Datum und Uhrzeit, auf die zurückgesetzt werden soll. Die Wiederherstellung kann einige Minuten dauern."}
   :rec/date {:en "Date" :de "Datum"}
   :rec/time {:en "Time" :de "Uhrzeit"}
   :rec/what-head {:en "Select the items to recover." :de "Wählen Sie die wiederherzustellenden Elemente aus."}
   :rec/partitions {:en "Partitions" :de "Partitionen"}
   :rec/mbr {:en "MBR and Track 0" :de "MBR und Spur 0"}
   :rec/files-head {:en "Select the files and folders to recover." :de "Wählen Sie die wiederherzustellenden Dateien und Ordner aus."}
   :rec/unreadable {:en "Not readable: %s" :de "Nicht lesbar: %s"}
   :rec/disk-head {:en "Select destination of Disk %d" :de "Wählen Sie das Ziel für Laufwerk %d aus."}
   :rec/disk-props {:en "Disk properties" :de "Laufwerkseigenschaften"}
   :rec/props-text {:en "Device: %s\nModel: %s\nCapacity: %s\nPartition scheme: %s\nSector size: %s bytes\nPartitions: %d"
                    :de "Gerät: %s\nModell: %s\nKapazität: %s\nPartitionsschema: %s\nSektorgröße: %s Byte\nPartitionen: %d"}
   :rec/signature {:en "Recover disk signature" :de "Datenträgersignatur wiederherstellen"}
   :rec/signature-tip {:en "Keeps the signature of the backed-up disk. Windows finds its boot disk by it."
                       :de "Behält die Signatur der gesicherten Platte. Windows findet sein Startlaufwerk darüber."}
   :rec/part-head {:en "Where should %s be recovered to?" :de "Wohin soll %s zurückgespielt werden?"}
   :rec/part-text {:en "Backed-up partition: %s, %s. The destination must be at least that large."
                   :de "Gesicherte Partition: %s, %s. Das Ziel muss mindestens so groß sein."}
   :rec/existing-parts {:en "Existing partitions" :de "Vorhandene Partitionen"}
   :rec/head-head {:en "Select the disk to write MBR and Track 0 to." :de "Wählen Sie die Platte aus, auf die MBR und Spur 0 geschrieben werden."}
   :rec/disks {:en "Disks" :de "Laufwerke"}
   :rec/files-dest-head {:en "Select the destination for the recovered files." :de "Wählen Sie das Ziel für die wiederhergestellten Dateien aus."}
   :rec/new-location {:en "New location" :de "Neuer Speicherort"}
   :rec/opt-skip-zeros {:en "Do not write empty blocks (faster on a new or wiped disk)"
                        :de "Leere Blöcke nicht schreiben (schneller auf einer neuen oder gelöschten Platte)"}
   :rec/opt-shutdown {:en "Shut down the computer after the recovery" :de "Computer nach Abschluss der Wiederherstellung herunterfahren"}
   :rec/op-files {:en "Recovering files: %s" :de "Dateien wiederherstellen: %s"}
   :rec/op-disk {:en "Recovering %s" :de "%s wiederherstellen"}
   :rec/op-part {:en "Copying partition: %s" :de "Partition kopieren: %s"}
   :rec/op-head {:en "Recovering MBR and Track 0" :de "MBR und Spur 0 wiederherstellen"}
   :rec/k-source {:en "Source" :de "Quelle"}
   :rec/k-dest {:en "Destination" :de "Ziel"}
   :rec/k-hard-disk {:en "Hard disk" :de "Festplatte"}
   :rec/k-steps {:en "Steps" :de "Schritte"}
   :rec/v-steps {:en "Clearing disk, copying partitions, recovering partition structure"
                 :de "Platte leeren, Partitionen kopieren, Partitionsstruktur wiederherstellen"}
   :rec/k-partition {:en "Partition" :de "Partition"}
   :rec/k-signature {:en "Disk signature" :de "Datenträgersignatur"}
   :rec/v-sig-keep {:en "recover" :de "wiederherstellen"}
   :rec/v-sig-new {:en "create new" :de "neu erzeugen"}
   :rec/k-letter {:en "Drive letter" :de "Laufwerksbuchstabe"}
   :rec/k-fs {:en "File system" :de "Dateisystem"}
   :rec/k-label {:en "Volume label" :de "Volumebezeichnung"}
   :rec/k-size {:en "Size" :de "Größe"}
   :rec/k-extent {:en "Extent" :de "Umfang"}
   :rec/v-before-first {:en "%s before the first partition" :de "%s vor der ersten Partition"}
   :rec/v-files-source {:en "%s, partition %s: %s" :de "%s, Partition %s: %s"}
   :rec/e-archive {:en "Select a backup." :de "Wählen Sie eine Sicherung aus."}
   :rec/e-items {:en "Select at least one item." :de "Wählen Sie mindestens ein Element aus."}
   :rec/e-files {:en "Select at least one file or folder." :de "Wählen Sie mindestens eine Datei oder einen Ordner aus."}
   :rec/e-folder {:en "Enter an existing folder as the destination." :de "Geben Sie einen vorhandenen Ordner als Ziel an."}
   :rec/e-disk {:en "Select a destination disk." :de "Wählen Sie ein Ziellaufwerk aus."}
   :rec/e-small {:en "At %s the destination is smaller than the backed-up disk (%s)."
                 :de "Das Ziel ist mit %s kleiner als die gesicherte Platte (%s)."}
   :rec/e-mounted {:en "A file system on %s is mounted right now. Select another disk."
                   :de "Auf %s ist gerade ein Dateisystem eingehängt. Wählen Sie eine andere Platte."}
   :rec/confirm-wipe {:en "The selected destination disk contains partitions that might hold data.\n\nClick OK to delete all partitions on the destination disk. Nothing is deleted until you click Proceed on the last page."
                      :de "Die ausgewählte Zielfestplatte enthält Partitionen, auf denen möglicherweise Daten gespeichert sind.\n\nKlicken Sie auf OK, um alle Partitionen auf der Zielfestplatte zu löschen. Gelöscht wird erst, wenn Sie auf der letzten Seite auf „Fortfahren“ klicken."}
   :rec/e-part {:en "Select the destination partition." :de "Wählen Sie die Zielpartition aus."}
   :rec/e-part-small {:en "The destination partition is smaller than the backed-up one." :de "Die Zielpartition ist kleiner als die gesicherte."}
   :rec/e-part-mounted {:en "%s is mounted (%s). Unmount it first." :de "%s ist eingehängt (%s). Bitte erst aushängen."}
   :rec/e-head {:en "Select a disk." :de "Wählen Sie ein Laufwerk aus."}
   :rec/h-archive {:en "Select the backup. \"Browse …\" sets where the repository is."
                   :de "Wählen Sie die Sicherung. Über „Durchsuchen …“ stellen Sie ein, wo das Repository liegt."}
   :rec/h-point {:en "Days with a backup are marked green. Pick the time on the right."
                 :de "Grün markierte Tage haben eine Sicherung. Wählen Sie rechts die Uhrzeit."}
   :rec/h-what {:en "Tick the disk to recover all of it, or single partitions."
                :de "Haken Sie die Platte an, um sie ganz wiederherzustellen, oder einzelne Partitionen."}
   :rec/h-finish {:en "Check the operations. Nothing is written until you click Proceed."
                  :de "Prüfen Sie die Vorgänge. Erst „Fortfahren“ schreibt auf das Ziel."}
   :rec/running {:en "Recovery" :de "Wiederherstellung"}
   :rec/success {:en "The data was successfully recovered." :de "Die Daten wurden erfolgreich wiederhergestellt."}
   ;; --- backup wizard
   :bak/title {:en "Backup Wizard" :de "Sicherungsassistent"}
   :bak/source {:en "Source" :de "Quelle"}
   :bak/dest {:en "Destination" :de "Ziel"}
   :bak/options {:en "Backup options" :de "Sicherungsoptionen"}
   :bak/disks-head {:en "Select the disks and partitions to back up." :de "Wählen Sie die zu sichernden Laufwerke und Partitionen aus."}
   :bak/disks-parts {:en "Disks and partitions" :de "Laufwerke und Partitionen"}
   :bak/files-head {:en "Select the files and folders to back up." :de "Wählen Sie die zu sichernden Dateien und Ordner aus."}
   :bak/computer {:en "Computer" :de "Arbeitsplatz"}
   :bak/dest-head {:en "Select the backup destination." :de "Wählen Sie das Ziel der Sicherung aus."}
   :bak/new-archive {:en "Create new backup archive" :de "Neues Sicherungsarchiv erstellen"}
   :bak/repository {:en "Repository:" :de "Repository:"}
   :bak/archive-name {:en "Archive name:" :de "Archivname:"}
   :bak/init {:en "Create the repository if it does not exist yet" :de "Repository anlegen, falls es noch nicht besteht"}
   :bak/reading {:en "Reading existing backups …" :de "Vorhandene Sicherungen werden gelesen …"}
   :bak/empty {:en "The repository is empty." :de "Das Repository ist leer."}
   :bak/count {:en "%d backups in the repository, latest %s. Equal data is stored only once."
               :de "%d Sicherungen im Repository, zuletzt %s. Gleiche Daten werden nur einmal abgelegt."}
   :bak/unreachable {:en "Repository not reachable: %s" :de "Repository nicht erreichbar: %s"}
   :bak/op-files {:en "Backing up files to %s" :de "Dateien sichern nach %s"}
   :bak/op-disks {:en "Backing up disks to %s" :de "Laufwerke sichern nach %s"}
   :bak/op-dry {:en "Dry run: backing up disks to %s" :de "Probelauf: Laufwerke sichern nach %s"}
   :bak/k-compression {:en "Compression" :de "Kompression"}
   :bak/k-block {:en "Block size" :de "Blockgröße"}
   :bak/compression {:en "Compression:" :de "Kompression:"}
   :bak/block {:en "Block size:" :de "Blockgröße:"}
   :bak/raw {:en "Read every sector, ignore allocation (--raw)" :de "Jeden Sektor lesen, Belegung nicht beachten (--raw)"}
   :bak/dry-run {:en "Dry run – open the sources and show what would be read, write nothing"
                 :de "Probelauf – Quellen öffnen und zeigen, was gelesen würde, nichts schreiben"}
   :bak/shutdown {:en "Shut down the computer after the backup" :de "Computer nach Abschluss der Sicherung herunterfahren"}
   :bak/volumes {:en "Volumes" :de "Volumes"}
   :bak/e-files {:en "Select at least one file or folder." :de "Wählen Sie mindestens eine Datei oder einen Ordner aus."}
   :bak/e-disks {:en "Select at least one disk or partition." :de "Wählen Sie mindestens ein Laufwerk oder eine Partition aus."}
   :bak/e-repo {:en "Enter where the repository is." :de "Geben Sie an, wo das Repository liegt."}
   :bak/e-name {:en "Enter an archive name." :de "Geben Sie einen Archivnamen an."}
   :bak/e-taken {:en "The repository already holds an archive \"%s\". Choose another name."
                 :de "Im Repository gibt es schon ein Archiv „%s“. Wählen Sie einen anderen Namen."}
   :bak/running {:en "Backup" :de "Sicherung"}
   :bak/success {:en "The backup was created successfully." :de "Die Sicherung wurde erfolgreich erstellt."}})

(defn- kernel-lang []
  (try
    (some->> (slurp "/proc/cmdline")
             (re-find #"(?:^|\s)borgclj\.lang=(\w+)")
             second)
    (catch Throwable _ nil)))

(defn detect
  "The language to use: :en or :de."
  []
  (let [want (str/lower-case (str (or (System/getenv "BORGCLJ_LANG") (kernel-lang)
                                      (.getLanguage (Locale/getDefault)))))]
    (if (str/starts-with? want "de") :de :en)))

(def lang (atom :en))

(defn set-lang!
  "Picks the language and makes the JDK's own dialogs follow it."
  [l]
  (reset! lang l)
  (Locale/setDefault (if (= :de l) Locale/GERMANY Locale/US)))

(defn t
  "The text for k in the current language; extra args go through format."
  ^String [k & args]
  (let [e (or (get strings k) (throw (ex-info (str "no text for " k) {:key k})))
        s (or (get e @lang) (:en e))]
    (if (seq args) (apply format s args) s)))
