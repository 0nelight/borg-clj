(ns borgclj.gui
  "The interface as a window: Swing with FlatLaf, from the same program on
   Windows and in the recovery system.

   Layout and flow follow the Acronis True Image rescue media: on the left the
   blue bar Home / Backup / Recovery / Log / Tools & Utilities, on the right
   \"What would you like to do?\" with Back Up and Recover, each for My Disks or
   Files & Folders. From there wizards (borgclj.gui.backup, borgclj.gui.recovery)
   lead step by step to the goal.

   Swing draws every control itself instead of borrowing the operating
   system's. That is why the window looks the same on Windows and under X in
   the recovery system -- a bare X server with a small window manager is all it
   needs there, no browser and no HTTP."
  (:require [borgclj.gui.backup :as backup]
            [borgclj.gui.connect :as connect]
            [borgclj.gui.i18n :as i18n :refer [t]]
            [borgclj.gui.look :as look]
            [borgclj.gui.progress :as progress]
            [borgclj.gui.recovery :as recovery]
            [borgclj.gui.widgets :as w]
            [borgclj.log :as log]
            [borgclj.ui.config :as config]
            [borgclj.ui.job :as job]
            [borgclj.ui.pages :as pages]
            [clojure.string :as str])
  (:import (com.formdev.flatlaf FlatLightLaf)
           (java.awt BorderLayout Color Component Dimension FlowLayout Font
                     GraphicsEnvironment Window)
           (java.awt.event ActionListener)
           (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.time.format DateTimeFormatter FormatStyle)
           (javax.swing BorderFactory Box BoxLayout JButton JComponent JDialog JFrame JLabel
                        JMenuItem JPanel JPopupMenu JScrollPane JSeparator JSplitPane JTextArea
                        SwingUtilities UIManager WindowConstants)))

(set! *warn-on-reflection* true)

(defn- linux? []
  (str/starts-with? (str (System/getProperty "os.name")) "Linux"))

(defn- heading ^JLabel [^String s]
  (doto (JLabel. s)
    (.setFont (look/font 19 (bit-or Font/BOLD Font/ITALIC)))
    (look/pad! 18 16 14 0)))

;; --- parts of the home page -----------------------------------------------------

(defn- action-row
  "Icon, title and below it the links \"My Disks | Files & Folders\"."
  ^JPanel [icon ^String title links]
  (let [text (JPanel. (FlowLayout. FlowLayout/LEFT 0 0))
        col (w/vbox (doto (JLabel. title) (.setFont (look/font 17))) text)]
    (.setOpaque text false)
    (doseq [[i [s f]] (map-indexed vector links)]
      (when (pos? i) (.add text (doto (JLabel. "  |  ") (.setForeground (look/link-blue)))))
      (.add text (look/link s f)))
    (doto (JPanel. (BorderLayout. 12 0))
      (.setOpaque false)
      (.setBorder (BorderFactory/createEmptyBorder 10 10 10 10))
      (.add (JLabel. ^javax.swing.Icon icon) BorderLayout/WEST)
      (.add col BorderLayout/CENTER)
      (.setMaximumSize (Dimension. 700 80)))))

(defn- start-page ^JPanel [owner app]
  (w/vbox
   (heading (t :app/welcome))
   (doto (JLabel. (t :app/what))
     (.setFont (look/font 19 Font/ITALIC))
     (look/pad! 0 16 10 0))
   (action-row (look/big-disk-icon true) (t :app/backup)
               [[(t :app/my-disks) #(backup/start! (owner) app :disks)]
                [(t :app/files) #(backup/start! (owner) app :files)]])
   (action-row (look/big-disk-icon false) (t :app/recover)
               [[(t :app/my-disks) #(recovery/start! (owner) app :disks)]
                [(t :app/files) #(recovery/start! (owner) app :files)]])
   (doto (JSeparator.) (.setMaximumSize (Dimension. 470 2)) (look/pad! 8 16 8 0))
   (when (linux?)
     (doto (JLabel. (str "<html>" (t :app/letters) "</html>"))
       (.setIcon look/info-icon)
       (.setFont (look/font 12 Font/ITALIC))
       (look/pad! 8 16 0 0)))))

(defn- section-page
  "The Backup and Recovery pages: the same two ways, with a sentence each."
  ^JPanel [^String title rows]
  (apply w/vbox
         (heading title)
         (for [[icon ^String head ^String text f] rows]
           (action-row icon head [[text f]]))))

;; --- log ----------------------------------------------------------------------------

(defn- log-page ^JComponent [app]
  (let [rows (atom (vec (reverse @(:history app))))
        stamp (DateTimeFormatter/ofLocalizedDateTime FormatStyle/MEDIUM)
        detail (doto (JTextArea.) (.setEditable false) (.setFont (Font. Font/MONOSPACED Font/PLAIN 12)))
        tbl (w/row-table
             [{:title (t :log/time) :width 150 :get #(.format ^java.time.LocalDateTime (:time %) stamp)}
              {:title (t :log/operation) :width 360 :get :title}
              {:title (t :log/result) :width 120 :get #(case (:state %)
                                                         :done (t :log/done) :failed (t :log/failed)
                                                         :cancelled (t :log/cancelled) (str (:state %)))}]
             rows
             {:on-select (fn [r] (.setText detail (str/join "\n" (:log r))) (.setCaretPosition detail 0))})]
    (if (empty? @rows)
      (w/vbox (heading (t :nav/log))
              (doto (JLabel. (t :log/empty)) (look/pad! 0 16 0 0)))
      (doto (JPanel. (BorderLayout.))
        (.setOpaque false)
        (.add (heading (t :nav/log)) BorderLayout/NORTH)
        (.add (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT (w/framed (t :log/operations) tbl)
                                 (doto (JScrollPane. detail) (.setMinimumSize (Dimension. 100 150))))
                (.setResizeWeight 0.5)
                (.setDividerLocation 220)
                (look/pad! 0 16 16 16))
              BorderLayout/CENTER)))))

;; --- tools & utilities ------------------------------------------------------------------

(defn- disk-rows [app]
  (let [d (pages/disks app false)]
    (vec (mapcat (fn [i dk]
                   (cons {:group true
                          :name (str (t :c/disk-n (inc (long i))) " (" (or (:name dk) (:disk dk)) ")")
                          :cap (log/human (long (:length dk 0))) :model (or (:model dk) "")
                          :type (name (or (:style dk) :raw))}
                         (for [p (:partitions dk)]
                           {:name (or (:name p) (str "#" (:number p))) :cap (log/human (long (:length p 0)))
                            :model (or (:label p) "") :type (or (:fstype p) "")})))
                 (range) (:disks d)))))

(defn- disks-panel ^JComponent [app]
  (w/async #(pages/disks app true)
           (fn [_]
             (let [rows (atom (disk-rows app))]
               (w/framed (t :disks/this)
                         (w/row-table [{:title (t :disks/col-disk) :width 220 :get :name}
                                       {:title (t :c/capacity) :width 90 :align :right :get :cap}
                                       {:title (t :disks/col-model) :width 220 :get :model}
                                       {:title (t :disks/col-kind) :width 80 :get :type}]
                                      rows {}))))))

(defn- tools-page ^JComponent [owner app show-disks]
  (w/vbox
   (heading (t :nav/tools))
   (action-row (look/disk-icon 40) (t :tools/repository)
               [[(t :tools/repository-link) #(connect/edit! (owner) (:config app))]])
   (action-row (look/disk-icon 40) (t :nav/disks)
               [[(t :tools/disks-link) show-disks]])
   (action-row look/check-icon (t :tools/verify)
               [[(t :tools/verify-link)
                 (fn []
                   (let [c @(:config app)]
                     (progress/run-ops! (owner) app
                                        {:title (t :tools/verify-title)
                                         :ops [{:title (t :tools/verify-op) :kind :verify
                                                :argv (into ["verify" (config/location c nil)]
                                                            (config/connection-args c))}]
                                         :success (t :tools/verify-ok)})))]])))

;; --- main window ------------------------------------------------------------------------

(defn- nav-item
  "An entry of the left bar: a blue tile, the selected one white."
  ^JPanel [^String s selected? f]
  (let [l (JLabel. s)
        p (proxy [JPanel] []
            (paintComponent [^java.awt.Graphics g]
              (let [^JPanel this this
                    g2 ^java.awt.Graphics2D (.create g)]
                (.setRenderingHint g2 java.awt.RenderingHints/KEY_ANTIALIASING
                                   java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
                (.setPaint g2 (if selected?
                                (java.awt.GradientPaint. 0 0 Color/WHITE 0 (.getHeight this) (look/rgb 0xeef3fa))
                                (java.awt.GradientPaint. 0 0 (look/rgb 0x4a7fbe) 0 (.getHeight this) (look/rgb 0x2d5f9e))))
                (.fillRoundRect g2 0 0 (dec (.getWidth this)) (dec (.getHeight this)) 12 12)
                (.setColor g2 (Color. 255 255 255 90))
                (.drawRoundRect g2 0 0 (dec (.getWidth this)) (dec (.getHeight this)) 12 12)
                (.dispose g2))))]
    (.setFont l (look/font 18 Font/ITALIC))
    (.setForeground l (if selected? (look/rgb 0x1a1a1a) Color/WHITE))
    (doto p
      (.setOpaque false)
      (.setLayout (BorderLayout.))
      (.setBorder (BorderFactory/createEmptyBorder 16 14 16 10))
      (.add l BorderLayout/CENTER)
      (.setMaximumSize (Dimension. 400 58))
      (.setAlignmentX 0.0))
    (look/clickable! p f)
    p))

(defn- menu-button
  "A button of the top bar with a drop-down menu."
  ^JButton [^String s items]
  (let [b (JButton. (str s " ▾"))
        m (JPopupMenu.)]
    (doseq [[^String label f] items]
      (.add m (doto (JMenuItem. label)
                (.addActionListener (reify ActionListener (actionPerformed [_ _] (f)))))))
    (doto b
      (.setForeground Color/WHITE)
      (.setFont (look/font 13 Font/ITALIC))
      (.setContentAreaFilled false)
      (.setBorderPainted false)
      (.setFocusPainted false)
      (.addActionListener (reify ActionListener
                            (actionPerformed [_ _] (.show m b 0 (.getHeight b))))))))

(defn- window ^JFrame [app fullscreen]
  (let [f (JFrame. "borg-imager")
        owner (fn [] f)
        content (JPanel. (BorderLayout.))
        nav (look/gradient-panel (look/rgb 0x3569a8) (look/rgb 0x7fa9dc) true)
        current (atom :start)
        show! (atom nil)
        go! (fn [k] (reset! current k) (@show!))
        order [:start :backup :recovery :log :tools]
        nav-label {:start :nav/home :backup :nav/backup :recovery :nav/recovery
                   :log :nav/log :tools :nav/tools}
        page-of (fn [k]
                  (case k
                    :start (start-page owner app)
                    :backup (section-page (t :nav/backup)
                                          [[(look/big-disk-icon true) (t :app/my-disks) (t :sec/backup-disks)
                                            #(backup/start! f app :disks)]
                                           [(look/big-disk-icon true) (t :app/files) (t :sec/backup-files)
                                            #(backup/start! f app :files)]])
                    :recovery (section-page (t :nav/recovery)
                                            [[(look/big-disk-icon false) (t :app/my-disks) (t :sec/recover-disks)
                                              #(recovery/start! f app :disks)]
                                             [(look/big-disk-icon false) (t :app/files) (t :sec/recover-files)
                                              #(recovery/start! f app :files)]])
                    :log (log-page app)
                    :tools (tools-page owner app #(go! :disks))
                    :disks (doto (JPanel. (BorderLayout.)) (.setOpaque false)
                             (.add (heading (t :nav/disks)) BorderLayout/NORTH)
                             (.add (doto (JPanel. (BorderLayout.)) (.setOpaque false) (look/pad! 0 16 16 16)
                                         (.add (disks-panel app) BorderLayout/CENTER))
                                   BorderLayout/CENTER))))]
    (reset! show!
            (fn []
              (.removeAll nav)
              (.setLayout nav (BoxLayout. nav BoxLayout/Y_AXIS))
              (.setBorder nav (BorderFactory/createEmptyBorder 10 10 10 10))
              (doseq [k order]
                (.add nav (nav-item (t (nav-label k))
                                    (or (= k @current) (and (= k :tools) (= :disks @current)))
                                    #(go! k)))
                (.add nav (Box/createVerticalStrut 6)))
              (.removeAll content)
              (.add content ^Component (page-of @current) BorderLayout/NORTH)
              (doseq [^JComponent c [nav content]] (.revalidate c) (.repaint c))))
    (let [top (look/gradient-panel (look/rgb 0x2b5487) (look/rgb 0x173a66))
          main (JPanel. (BorderLayout.))]
      (doto top
        (.setLayout (FlowLayout. FlowLayout/LEFT 4 2))
        (.add (menu-button (t :nav/backup) [[(t :app/my-disks) #(backup/start! f app :disks)]
                                             [(t :app/files) #(backup/start! f app :files)]]))
        (.add (menu-button (t :nav/recovery) [[(t :app/my-disks) #(recovery/start! f app :disks)]
                                               [(t :app/files) #(recovery/start! f app :files)]]))
        (.add (menu-button (t :nav/tools) [[(t :menu/repository) #(connect/edit! f (:config app))]
                                            [(t :menu/disks) #(go! :disks)]
                                            [(t :nav/log) #(go! :log)]])))
      (.setPreferredSize nav (Dimension. 290 100))
      (.setBackground content Color/WHITE)
      (doto main
        (.add top BorderLayout/NORTH)
        (.add nav BorderLayout/WEST)
        (.add (doto (JScrollPane. (doto (JPanel. (BorderLayout.)) (.setBackground Color/WHITE)
                                        (.add content BorderLayout/NORTH)))
                (.setBorder (BorderFactory/createEmptyBorder)))
              BorderLayout/CENTER))
      (@show!)
      ;; after a wizard the log may have new entries
      (add-watch (:history app) ::new (fn [_ _ _ _] (SwingUtilities/invokeLater #(@show!))))
      (doto f
        (.setDefaultCloseOperation WindowConstants/EXIT_ON_CLOSE)
        (.setContentPane main)))
    (doto f
      (.setSize 1000 680)
      (.setLocationRelativeTo nil))
    ;; maximised, not frameless: the title bar stays, as in Acronis
    (when fullscreen (.setExtendedState f JFrame/MAXIMIZED_BOTH))
    f))

(defn- set-java-home!
  "The native program has no JDK beside it, but AWT wants java.home -- with a
   lib/ in it. Without lib/, the search for a font configuration gives up before
   it notes that there is none, then assumes a prepared one instead of asking
   fontconfig, and fails with \"Fontconfig head is null\". On Windows the JDK's
   fontconfig.bfc belongs there; on Linux an empty directory is enough. If there
   is none beside the program, one under tmp does."
  []
  (when-not (System/getProperty "java.home")
    (let [exe (.orElse (.command (.info (java.lang.ProcessHandle/current))) nil)
          dir (some-> ^String exe (Path/of (make-array String 0)) .toAbsolutePath .getParent)
          home (if (and dir (Files/isDirectory (.resolve ^Path dir "lib") (make-array LinkOption 0)))
                 dir
                 (let [tmp (Files/createTempDirectory "borgi-gui" (make-array FileAttribute 0))]
                   (Files/createDirectories (.resolve tmp "lib") (make-array FileAttribute 0))
                   tmp))]
      (System/setProperty "java.home" (str home)))))

(defn show!
  "Opens the window. Returns at once; the program ends when the window is
   closed. run is the command line's entry point: the wizards run their
   operations through it in the same process."
  [{:keys [fullscreen run]}]
  (set-java-home!)
  (i18n/set-lang! (i18n/detect))
  (when (GraphicsEnvironment/isHeadless)
    (throw (ex-info "no display: DISPLAY is not set or not reachable" {})))
  (UIManager/put "defaultFont" (Font. Font/SANS_SERIF Font/PLAIN 13))
  ;; FlatLaf's own native helper library is looked for beside its jar. A native
  ;; image has no jar, the path arithmetic fails, and the exception came out of
  ;; the first setVisible -- the recovery system showed a black screen. Without
  ;; the library FlatLaf draws title bars in plain Swing, the same everywhere.
  (System/setProperty "flatlaf.useNativeLibrary" "false")
  (FlatLightLaf/setup)
  ;; FlatLaf draws title bar and frame itself. In the recovery system there is
  ;; none otherwise (matchbox runs without title bars), and on Windows they
  ;; look the same this way -- closing, moving and resizing work alike.
  (JFrame/setDefaultLookAndFeelDecorated true)
  (JDialog/setDefaultLookAndFeelDecorated true)
  (let [app {:config (atom (try (config/load-config) (catch Throwable _ config/defaults)))
             :disks (atom nil)
             :job (job/new-job)
             :run run
             :history (atom [])}]
    (SwingUtilities/invokeLater
     (fn []
       (.setVisible (window app fullscreen) true)
       ;; For the netboot test and the journal: from here on the window is up.
       ;; In the recovery system the line also goes to the console and thus to
       ;; the serial port.
       (println "borgi gui: window open")
       (flush)))))
