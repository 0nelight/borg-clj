(ns borgclj.gui.recovery
  "The recovery wizard, step by step as in the Acronis True Image rescue media:

     Archive selection -> Recovery method -> Recovery point -> What to recover
     -> Destination of Disk N (or Settings of partition N, Destination of MBR
     and Track 0) -> Finish

   and for files a destination folder instead of a disk. What the summary lists
   are exactly the verbs that run afterwards -- both are built from the same
   state by `plan`.

   Terms: a backup with several versions in Acronis is a series of archives
   sharing a name here (tower-2026-09-20, tower-2026-09-27 ...); the recovery
   point picks one of them."
  (:require [borgclj.gui.connect :as connect]
            [borgclj.gui.data :as data]
            [borgclj.gui.i18n :refer [t]]
            [borgclj.gui.look :as look]
            [borgclj.gui.progress :as progress]
            [borgclj.gui.widgets :as w]
            [borgclj.gui.wizard :as wizard]
            [borgclj.log :as log]
            [borgclj.ui.config :as config]
            [borgclj.ui.pages :as pages]
            [clojure.string :as str])
  (:import (java.awt BorderLayout Dimension FlowLayout Font Window)
           (java.awt.event ActionListener)
           (java.io RandomAccessFile)
           (java.time LocalDateTime)
           (java.time.format DateTimeFormatter FormatStyle)
           (javax.swing BorderFactory Box ButtonGroup DefaultListModel JCheckBox JComponent
                        JEditorPane JFileChooser JLabel JList JOptionPane JPanel JRadioButton
                        JScrollPane JTextField JTree ListSelectionModel SwingUtilities)
           (javax.swing.event TreeExpansionEvent TreeWillExpandListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreeCellRenderer)))

(set! *warn-on-reflection* true)

(defn- cfg [s] @(:config (:app s)))
(defn- linux? [] (str/starts-with? (str (System/getProperty "os.name")) "Linux"))

(defn- when-text
  "Date and time in the style of the chosen language."
  ^String [^LocalDateTime tm]
  (if tm (.format tm (DateTimeFormatter/ofLocalizedDateTime FormatStyle/SHORT)) "—"))

(defn- time-text ^String [^LocalDateTime tm]
  (.format tm (DateTimeFormatter/ofLocalizedTime FormatStyle/MEDIUM)))

;; --- what the chosen archive holds ---------------------------------------------

(defn- layout-of
  "The images of the chosen archive; loaded the first time, then kept."
  [state]
  (let [a (:archive @state)]
    (or (get-in @state [:layouts a])
        (let [l (data/layout (cfg @state) a)]
          (swap! state assoc-in [:layouts a] l)
          l))))

(defn- cached-layout [s] (get-in s [:layouts (:archive s)]))

(defn- flag-text ^String [flags]
  (str/join "," (map #(t (keyword "c" (name %))) flags)))

(defn- part-name ^String [p]
  (str (or (:fs p) (t :c/partition))
       (when-let [l (:label p)] (str " (" l ")"))
       (when-let [l (:letter p)] (str " (" l ":)"))
       (when-not (or (:label p) (:letter p)) (when-let [n (:number p)] (str " #" n)))))

(defn- disk-label ^String [i] (t :c/disk-n (inc (long i))))

(defn- members
  "What can be ticked separately in image i: [i :head] and [i number]."
  [img i]
  (concat (when (and (= :disk (:kind img)) (pos? (long (or (:head img) 0)))) [[i :head]])
          (for [p (:parts img)] [i (or (:number p) 0)])))

(defn- whole? [s img i]
  (let [m (members img i)] (and (seq m) (every? (:sel s) m))))

;; --- destinations -----------------------------------------------------------------

(defn- targets [s force]
  (pages/disks (:app s) force))

(defn- disk-target ^String [dk] (or (:device dk) (str "disk:" (:disk dk))))
(defn- vol-target ^String [v] (or (:device v) (str (:letter v) ":")))

(defn- disk-title ^String [i dk]
  (str (disk-label i) (when-let [n (:name dk)] (str " (" n ")"))))

(defn- dest-steps
  "One destination step per whole disk, otherwise one per partition and one for
   MBR and Track 0."
  [s]
  (let [imgs (cached-layout s)]
    (vec
     (mapcat
      (fn [i img]
        (if (and (= :disk (:kind img)) (whole? s img i))
          [{:id [:disk i] :img i :kind :disk :label (t :rec/dest-disk (inc (long i)))}]
          (concat
           (for [p (:parts img) :let [pn (or (:number p) 0)] :when ((:sel s) [i pn])]
             {:id [:part i pn] :img i :pn pn :kind :part
              :label (t :rec/part-settings
                        (or (some-> (:letter p) (str ":")) (:label p) (str "#" pn)))})
           (when ((:sel s) [i :head])
             [{:id [:head i] :img i :kind :head :label (t :rec/dest-head)}]))))
      (range) imgs))))

;; --- operations ------------------------------------------------------------------

(defn- new-signature!
  "A new disk signature (MBR, offset 0x1B8) on the destination disk. Read and
   written as a whole sector, because Windows lets raw devices be written only
   in whole sectors."
  [^String dev]
  (let [path (if-let [[_ n] (re-matches #"(?i)disk:(\d+)" dev)]
               (str "\\\\.\\PhysicalDrive" n)
               dev)
        sig (let [r (java.security.SecureRandom.)]
              (loop [] (let [v (.nextInt r)] (if (zero? v) (recur) v))))]
    (with-open [f (RandomAccessFile. ^String path "rw")]
      (let [b (byte-array 512)]
        (.readFully f b)
        (doto (java.nio.ByteBuffer/wrap b)
          (.order java.nio.ByteOrder/LITTLE_ENDIAN)
          (.putInt 0x1B8 (int sig)))
        (.seek f 0)
        (.write f b)))))

(defn plan
  "The operations \"Proceed\" starts: [{:title :kind :argv :lines :after}]."
  [s]
  (let [c (cfg s)
        a (:archive s)
        imgs (cached-layout s)]
    (if (= :files (:method s))
      (vec (for [[i pn path] (sort-by str (:files s))
                 :let [img (nth imgs i)]]
             {:title (t :rec/op-files path)
              :kind :get
              :argv (config/fetch-args c {:archive a :path path :dest (:file-dest s)
                                          :image (:image img)
                                          :partition (when (pos? (long pn)) (str pn))})
              :lines [[(t :rec/k-source) (t :rec/v-files-source (disk-label i) pn path)]
                      [(t :rec/k-dest) (:file-dest s)]]}))
      (vec
       (mapcat
        (fn [st]
          (let [i (:img st)
                img (nth imgs i)
                tgt (get-in s [:targets (:id st)])]
            (case (:kind st)
              :disk
              [{:title (t :rec/op-disk (disk-label i))
                :kind :restore
                :argv (config/restore-args c {:archive a :target (disk-target tgt) :image (:image img)
                                              :skip-zeros (:skip-zeros s)})
                :after (when (and (= :mbr (:style img)) (not (:keep-sig s)))
                         #(new-signature! (disk-target tgt)))
                :lines (concat
                        [[(t :rec/k-hard-disk) (str (disk-label i) " – " (:image img) ", "
                                                    (log/human (long (:length img))))]
                         [(t :rec/k-dest) (str (:name tgt) " – " (disk-target tgt) ", "
                                               (log/human (long (:length tgt)))
                                               (when-let [m (:model tgt)] (str ", " m)))]
                         [(t :rec/k-steps) (t :rec/v-steps)]]
                        (for [p (:parts img)]
                          [(t :rec/k-partition) (str (part-name p) ", " (log/human (long (:length p)))
                                                     (when (seq (:flags p)) (str ", " (flag-text (:flags p)))))])
                        (when (= :mbr (:style img))
                          [[(t :rec/k-signature) (t (if (:keep-sig s) :rec/v-sig-keep :rec/v-sig-new))]]))}]
              :part
              (let [p (first (filter #(= (:pn st) (or (:number %) 0)) (:parts img)))]
                [{:title (t :rec/op-part (part-name p))
                  :kind :restore
                  :argv (config/restore-args c {:archive a :target (vol-target tgt) :image (:image img)
                                                :partition (when (pos? (long (:pn st))) (str (:pn st)))
                                                :skip-zeros (:skip-zeros s)})
                  :lines [[(t :rec/k-hard-disk) (str (inc (long i)))]
                          [(t :rec/k-letter) (str (or (some-> (:letter p) (str ":")) "-") " -> "
                                                  (or (some-> (:letter tgt) (str ":")) (:name tgt)))]
                          [(t :rec/k-fs) (str (:fs p) " -> " (:fs p))]
                          [(t :rec/k-label) (or (:label p) "-")]
                          [(t :rec/k-size) (str (log/human (long (:length p))) " -> "
                                                (log/human (long (:length tgt))))]
                          [(t :rec/k-dest) (vol-target tgt)]]}])
              :head
              [{:title (t :rec/op-head)
                :kind :restore
                :argv (-> ["restore-image" (config/location c a) "--image" (:image img)
                           "--range" (str "0:" (:head img)) "--to" (disk-target tgt)]
                          (into (config/connection-args c)))
                :lines [[(t :rec/k-hard-disk) (str (inc (long i)))]
                        [(t :rec/k-dest) (str (:name tgt) " – " (disk-target tgt))]
                        [(t :rec/k-extent) (t :rec/v-before-first (log/human (long (:head img))))]]}])))
        (dest-steps s))))))

;; --- pages -------------------------------------------------------------------------

(defn- archive-rows [archives]
  (vec (mapcat (fn [[series as]]
                 (cons {:group true :series series :name series :latest (:name (last as))}
                       (for [a (reverse as)]
                         (assoc a :created (when-text (:time a)) :method (t :rec/full)))))
               (sort-by key (group-by :series archives)))))

(defn- archive-page [state]
  (let [holder (JPanel. (BorderLayout.))
        path (JTextField. (str (:repo (cfg @state))))
        load! (atom nil)
        rows (atom [])]
    (reset! load!
            (fn []
              (.removeAll holder)
              (.add holder
                    (w/async
                     #(data/archives (cfg @state))
                     (fn [archives]
                       (swap! state assoc :archives archives)
                       (reset! rows (archive-rows archives))
                       (let [tbl (w/row-table
                                  [{:title (t :rec/col-name) :width 150
                                    :get #(if (:group %) (:name %) (:series %))
                                    :icon (fn [_] (look/disk-icon))}
                                   {:title (t :rec/col-created) :width 150 :get #(:created % "")}
                                   {:title (t :rec/col-comments) :width 70 :get (fn [_] "")}
                                   {:title (t :rec/col-method) :width 150 :get #(:method % "")}
                                   {:title (t :rec/col-path) :width 170 :get #(if (:group %) "" (:name %))}]
                                  rows
                                  {:on-select (fn [r]
                                                (swap! state assoc :series (:series r)
                                                       :archive (if (:group r) (:latest r) (:name r))))})]
                         (w/select-row! tbl #(and (not (:group %)) (= (:name %) (:archive @state))) @rows)
                         (if (empty? archives)
                           (look/centered (t :rec/empty-repo))
                           (w/framed (t :rec/details) tbl)))))
                    BorderLayout/CENTER)
              (.revalidate holder) (.repaint holder)))
    (@load!)
    (let [browse (w/button (t :c/browse)
                           (fn []
                             (when (connect/edit! (SwingUtilities/getWindowAncestor holder)
                                                  (:config (:app @state)))
                               (.setText path (str (:repo (cfg @state))))
                               (swap! state dissoc :archive :series :layouts)
                               (@load!))))]
      (.addActionListener path (reify ActionListener
                                 (actionPerformed [_ _]
                                   (swap! (:config (:app @state)) assoc :repo (str/trim (.getText path)))
                                   (swap! state dissoc :archive :series :layouts)
                                   (@load!))))
      (w/page (t :rec/archive-head)
              (doto (JPanel. (BorderLayout. 0 8))
                (.setOpaque false)
                (.add holder BorderLayout/CENTER)
                (.add (doto (JPanel. (BorderLayout. 6 0))
                        (.setOpaque false)
                        (.add (JLabel. (t :c/path)) BorderLayout/WEST)
                        (.add path BorderLayout/CENTER)
                        (.add browse BorderLayout/EAST))
                      BorderLayout/SOUTH))))))

(defn- method-page [state]
  (let [g (ButtonGroup.)
        radio (fn [^String s k hint]
                (let [r (JRadioButton. s (= k (:method @state)))]
                  (.add g r)
                  (.setOpaque r false)
                  (.setFont r (look/font 14 Font/BOLD))
                  (.addActionListener r (reify ActionListener
                                          (actionPerformed [_ _] (swap! state assoc :method k))))
                  (w/vbox r (when hint (doto (w/text hint) (look/pad! 0 26 8 0))))))]
    (w/page (t :rec/method-head)
            (doto (JPanel. (BorderLayout.))
              (.setOpaque false)
              (.add (w/vbox (radio (t :rec/method-disks) :disks nil)
                            (radio (t :rec/method-files) :files (t :rec/method-files-hint)))
                    BorderLayout/NORTH)))))

(defn- point-page [state]
  (let [s @state
        series (filter #(= (:series s) (:series %)) (:archives s))
        by-day (group-by #(some-> ^LocalDateTime (:time %) .toLocalDate) series)
        chosen (first (filter #(= (:archive s) (:name %)) series))
        day (atom (some-> ^LocalDateTime (:time chosen) .toLocalDate))
        times (DefaultListModel.)
        lst (JList. times)
        cal-holder (JPanel. (BorderLayout.))
        fill! (atom nil)]
    (reset! fill!
            (fn []
              (.removeAll cal-holder)
              (.add cal-holder
                    (w/calendar (set (keys by-day)) @day
                                (fn [d] (reset! day d)
                                  (when-let [a (last (get by-day d))]
                                    (swap! state assoc :archive (:name a)))
                                  (@fill!)))
                    BorderLayout/CENTER)
              (.clear times)
              (doseq [a (sort-by :time (get by-day @day))]
                (.addElement times a))
              (let [i (first (keep-indexed (fn [i a] (when (= (:name a) (:archive @state)) i))
                                           (sort-by :time (get by-day @day))))]
                (when i (.setSelectedIndex lst (int i))))
              (.revalidate cal-holder) (.repaint cal-holder)))
    (.setCellRenderer lst (proxy [javax.swing.DefaultListCellRenderer] []
                            (getListCellRendererComponent [l v i sel foc]
                              (let [^javax.swing.DefaultListCellRenderer this this]
                                (proxy-super getListCellRendererComponent l
                                             (str (time-text (:time v)) "   " (:name v))
                                             i sel foc)))))
    (.setSelectionMode lst ListSelectionModel/SINGLE_SELECTION)
    (.addListSelectionListener lst (reify javax.swing.event.ListSelectionListener
                                     (valueChanged [_ _]
                                       (when-let [a (.getSelectedValue lst)]
                                         (swap! state assoc :archive (:name a))))))
    (@fill!)
    (w/page (t :rec/point-head)
            (w/vbox (doto (w/text (t :rec/point-text)) (look/pad! 0 0 10 0))
                    (doto (JPanel. (FlowLayout. FlowLayout/LEFT 0 0))
                      (.setOpaque false)
                      (.add (w/vbox (JLabel. (t :rec/date)) cal-holder))
                      (.add (Box/createHorizontalStrut 8))
                      (.add (w/vbox (JLabel. (t :rec/time))
                                    (doto (JScrollPane. lst) (.setPreferredSize (Dimension. 260 210))))))))))

(defn- what-page [state]
  (w/async
   #(layout-of state)
   (fn [imgs]
     (let [rows (atom [])
           refresh! (fn []
                      (reset! rows
                              (vec (mapcat
                                    (fn [i img]
                                      (let [s @state]
                                        (cons {:group true :img i :name (str (disk-label i) " – " (:image img))
                                               :checked (whole? s img i)}
                                              (concat
                                               (for [p (:parts img) :let [pn (or (:number p) 0)]]
                                                 {:img i :key [i pn] :name (part-name p)
                                                  :checked (contains? (:sel s) [i pn])
                                                  :flags (flag-text (:flags p)) :cap (log/human (long (:length p)))
                                                  :used (some-> (:used p) long log/human)
                                                  :type (or (:fs p) (t :c/unknown))})
                                               (when (some #{[i :head]} (members img i))
                                                 [{:img i :key [i :head] :name (t :rec/mbr)
                                                   :checked (contains? (:sel s) [i :head])
                                                   :type (t :rec/mbr)}])))))
                                    (range) imgs))))
           _ (refresh!)
           tbl (w/row-table
                [{:title "" :check true :width 28 :get #(boolean (:checked %))}
                 {:title (t :c/partition) :width 260 :get :name
                  :icon #(when-not (:group %) (look/disk-icon))}
                 {:title (t :c/flags) :width 70 :get #(:flags % "")}
                 {:title (t :c/capacity) :width 90 :align :right :get #(:cap % "")}
                 {:title (t :c/used) :width 100 :align :right :get #(or (:used %) "")}
                 {:title (t :c/type) :width 110 :get #(:type % "")}]
                rows
                {:on-toggle (fn [r on]
                              (let [ks (if (:group r)
                                         (members (nth imgs (:img r)) (:img r))
                                         [(:key r)])]
                                (swap! state update :sel
                                       (fn [sel] (if on (into (or sel #{}) ks)
                                                     (reduce disj (or sel #{}) ks))))
                                (refresh!)))})]
       (w/page (t :rec/what-head)
               (w/framed (t :rec/partitions) tbl))))))

;; --- file tree ----------------------------------------------------------------

(defn- file-node ^DefaultMutableTreeNode [m]
  (let [n (DefaultMutableTreeNode. m)]
    (when (:dir? m) (.add n (DefaultMutableTreeNode. {:placeholder true})))
    n))

(defn- files-page [state]
  (w/async
   #(layout-of state)
   (fn [imgs]
     (let [root (DefaultMutableTreeNode. {:name ""})
           model (DefaultTreeModel. root)
           tree (JTree. model)
           check (JCheckBox.)
           label (JLabel.)
           cell (JPanel. (FlowLayout. FlowLayout/LEFT 2 0))]
       (doseq [[i img] (map-indexed vector imgs)
               p (:parts img)
               :when (= "NTFS" (:fs p))]
         (.add root (file-node {:img i :pn (or (:number p) 0) :path "/" :dir? true
                                :name (str (disk-label i) " – " (part-name p))})))
       (.reload model)
       (.setOpaque cell false)
       (.add cell check) (.add cell label)
       (.setOpaque check false)
       (.setCellRenderer tree
                         (reify TreeCellRenderer
                           (getTreeCellRendererComponent [_ _ v _ _ _ _ _]
                             (let [m (.getUserObject ^DefaultMutableTreeNode v)
                                   k [(:img m) (:pn m) (:path m)]]
                               (.setSelected check (contains? (:files @state) k))
                               (.setVisible check (some? (:path m)))
                               (.setText label (str (:name m)
                                                    (when-let [z (:size m)] (str "   " (log/human (long z))))))
                               (.setIcon label (if (:dir? m) look/folder-icon look/file-icon))
                               cell))))
       (.addTreeWillExpandListener
        tree
        (reify TreeWillExpandListener
          (treeWillCollapse [_ _])
          (treeWillExpand [_ e]
            (let [^DefaultMutableTreeNode n (.getLastPathComponent (.getPath ^TreeExpansionEvent e))
                  m (.getUserObject n)]
              (when (and (= 1 (.getChildCount n))
                         (:placeholder (.getUserObject ^DefaultMutableTreeNode (.getChildAt n 0))))
                (.removeAllChildren n)
                (try
                  (doseq [en (data/entries (cfg @state) {:archive (:archive @state)
                                                         :image (:image (nth imgs (:img m)))
                                                         :partition (when (pos? (long (:pn m))) (:pn m))
                                                         :path (:path m)})]
                    (.add n (file-node {:img (:img m) :pn (:pn m) :dir? (:dir? en) :size (:size en)
                                        :name (:name en)
                                        :path (str (str/replace (:path m) #"/$" "") "/" (:name en))})))
                  (catch Throwable ex
                    (.add n (DefaultMutableTreeNode. {:name (t :rec/unreadable (ex-message ex))}))))
                (.nodeStructureChanged model n))))))
       (.addMouseListener
        tree
        (proxy [java.awt.event.MouseAdapter] []
          (mouseClicked [^java.awt.event.MouseEvent e]
            (when-let [tp (.getPathForLocation tree (.getX e) (.getY e))]
              (let [b (.getPathBounds tree tp)
                    m (.getUserObject ^DefaultMutableTreeNode (.getLastPathComponent tp))]
                (when (and (:path m) (< (.getX e) (+ (.x b) 22)))
                  (swap! state update :files
                         (fn [f] (let [f (or f #{}) k [(:img m) (:pn m) (:path m)]]
                                   (if (contains? f k) (disj f k) (conj f k)))))
                  (.repaint tree)))))))
       (.setRootVisible tree false)
       (.setShowsRootHandles tree true)
       (.setRowHeight tree 22)
       (w/page (t :rec/files-head)
               (w/framed (t :c/files-folders) tree))))))

;; --- destinations ------------------------------------------------------------------

(defn- disk-rows
  "The disks of this computer; those that are too small greyed out."
  ([s] (disk-rows s 0))
  ([s min-length]
   (vec (map-indexed (fn [i dk] (assoc dk :row-title (disk-title i dk)
                                       :disabled (< (long (:length dk 0)) (long min-length))))
                     (:disks (targets s false))))))

(defn- cumulative
  "Partitions with a start. lsblk names no offset; for the bar it is enough to
   lay them one after the other."
  [parts]
  (loop [[p & more] parts at (* 1024 1024) out []]
    (if-not p out
            (let [o (long (or (:offset p) at))]
              (recur more (+ o (long (:length p))) (conj out (assoc p :offset o)))))))

(defn- disk-page [state st]
  (let [img (nth (cached-layout @state) (:img st))
        rows (atom (disk-rows @state (:length img)))
        mapbox (JPanel. (BorderLayout.))
        show-map! (fn [dk]
                    (.removeAll mapbox)
                    (when dk
                      (.add mapbox (w/disk-map (:length dk)
                                               (cumulative (map #(assoc % :fs (:fstype %)) (:partitions dk))))
                            BorderLayout/CENTER))
                    (.revalidate mapbox) (.repaint mapbox))
        tbl (w/row-table
             [{:title (t :c/drive) :width 170 :get :row-title :icon (fn [_] (look/disk-icon))}
              {:title (t :c/capacity) :width 90 :align :right :get #(log/human (long (:length % 0)))}
              {:title (t :c/model) :width 170 :get #(or (:model %) "")}
              {:title (t :c/interface) :width 90 :get #(or (some-> (:interface %) str/upper-case) "")}]
             rows
             {:on-select (fn [dk] (swap! state assoc-in [:targets (:id st)] dk) (show-map! dk))})
        sig (JCheckBox. (t :rec/signature) (boolean (:keep-sig @state)))
        props (look/white-label (t :rec/disk-props))]
    (look/clickable! props
                     (fn []
                       (when-let [dk (get-in @state [:targets (:id st)])]
                         (JOptionPane/showMessageDialog
                          tbl (t :rec/props-text (disk-target dk) (or (:model dk) "—")
                                 (log/human (long (:length dk 0))) (name (or (:style dk) :raw))
                                 (str (:sector-size dk)) (count (:partitions dk)))
                          (t :rec/disk-props) JOptionPane/INFORMATION_MESSAGE))))
    (.setOpaque sig false)
    (.setEnabled sig (= :mbr (:style img)))
    (.setToolTipText sig (t :rec/signature-tip))
    (.addActionListener sig (reify ActionListener
                              (actionPerformed [_ _] (swap! state assoc :keep-sig (.isSelected sig)))))
    (w/select-row! tbl #(= (:device %) (:device (get-in @state [:targets (:id st)]))) @rows)
    (show-map! (get-in @state [:targets (:id st)]))
    (w/page (t :rec/disk-head (inc (long (:img st))))
            (doto (JPanel. (BorderLayout. 0 6))
              (.setOpaque false)
              (.add (w/framed props tbl) BorderLayout/CENTER)
              (.add (w/vbox sig mapbox (w/legend)) BorderLayout/SOUTH)))))

(defn- volume-rows [s]
  (vec (for [v (:volumes (targets s false)) :when (not (:error v))]
         (assoc v :row-title (or (some-> (:letter v) (str (when-not (linux?) ":"))) (:name v))))))

(defn- part-page [state st]
  (let [img (nth (cached-layout @state) (:img st))
        p (first (filter #(= (:pn st) (or (:number %) 0)) (:parts img)))
        rows (atom (volume-rows @state))
        tbl (w/row-table
             [{:title (t :c/volume) :width 140 :get :row-title :icon (fn [_] (look/disk-icon))}
              {:title (t :c/capacity) :width 90 :align :right :get #(log/human (long (:length % 0)))}
              {:title (t :c/file-system) :width 100 :get #(or (:fstype %) (when (:ntfs %) "ntfs") "")}
              {:title (t :c/label) :width 150 :get #(or (:label %) "")}
              {:title (t :c/mounted) :width 120 :get #(or (:mountpoint %) "")}]
             rows
             {:on-select (fn [v] (swap! state assoc-in [:targets (:id st)] v))})]
    (w/select-row! tbl #(= (vol-target %) (some-> (get-in @state [:targets (:id st)]) vol-target)) @rows)
    (w/page (t :rec/part-head (part-name p))
            (doto (JPanel. (BorderLayout. 0 6))
              (.setOpaque false)
              (.add (JLabel. (t :rec/part-text (part-name p) (log/human (long (:length p)))))
                    BorderLayout/NORTH)
              (.add (w/framed (t :rec/existing-parts) tbl) BorderLayout/CENTER)))))

(defn- head-page [state st]
  (let [rows (atom (disk-rows @state))
        tbl (w/row-table
             [{:title (t :c/drive) :width 170 :get :row-title :icon (fn [_] (look/disk-icon))}
              {:title (t :c/capacity) :width 90 :align :right :get #(log/human (long (:length % 0)))}
              {:title (t :c/model) :width 200 :get #(or (:model %) "")}]
             rows
             {:on-select (fn [dk] (swap! state assoc-in [:targets (:id st)] dk))})]
    (w/select-row! tbl #(= (:device %) (:device (get-in @state [:targets (:id st)]))) @rows)
    (w/page (t :rec/head-head) (w/framed (t :rec/disks) tbl))))

(defn- files-dest-page [state]
  (let [f (JTextField. (str (:file-dest @state)) 40)
        browse (w/button (t :c/browse)
                         (fn []
                           (let [ch (doto (JFileChooser. (str (:file-dest @state)))
                                      (.setFileSelectionMode JFileChooser/DIRECTORIES_ONLY))]
                             (when (= JFileChooser/APPROVE_OPTION (.showOpenDialog ch f))
                               (.setText f (str (.getSelectedFile ch)))
                               (swap! state assoc :file-dest (str (.getSelectedFile ch)))))))]
    (.addCaretListener f (reify javax.swing.event.CaretListener
                           (caretUpdate [_ _] (swap! state assoc :file-dest (.getText f)))))
    (w/page (t :rec/files-dest-head)
            (doto (JPanel. (BorderLayout.))
              (.setOpaque false)
              (.add (w/vbox (doto (JRadioButton. (t :rec/new-location) true) (.setOpaque false))
                            (doto (JPanel. (BorderLayout. 6 0))
                              (.setOpaque false) (look/pad! 6 24 0 0)
                              (.add f BorderLayout/CENTER) (.add browse BorderLayout/EAST)))
                    BorderLayout/NORTH)))))

(defn- options-page [state]
  (let [cb (fn [^String s k]
             (let [c (JCheckBox. s (boolean (k @state)))]
               (.setOpaque c false)
               (.addActionListener c (reify ActionListener
                                       (actionPerformed [_ _] (swap! state assoc k (.isSelected c)))))
               c))]
    (w/page (t :rec/options)
            (doto (JPanel. (BorderLayout.))
              (.setOpaque false)
              (.add (w/vbox (cb (t :rec/opt-skip-zeros) :skip-zeros)
                            (cb (t :rec/opt-shutdown) :shutdown))
                    BorderLayout/NORTH)))))

;; --- checks before leaving a step --------------------------------------------------

(defn- mounted? [dk] (or (:mountpoint dk) (some :mountpoint (:partitions dk))))

(defn- leave-disk [state st]
  (let [s @state
        img (nth (cached-layout s) (:img st))
        dk (get-in s [:targets (:id st)])]
    (cond
      (nil? dk) (t :rec/e-disk)
      (< (long (:length dk 0)) (long (:length img)))
      (t :rec/e-small (log/human (long (:length dk 0))) (log/human (long (:length img))))
      (mounted? dk) (t :rec/e-mounted (disk-target dk))
      (and (seq (:partitions dk))
           (not (wizard/confirm? nil (t :rec/title) (t :rec/confirm-wipe))))
      :stay
      :else nil)))

(defn- leave-part [state st]
  (let [s @state
        img (nth (cached-layout s) (:img st))
        p (first (filter #(= (:pn st) (or (:number %) 0)) (:parts img)))
        v (get-in s [:targets (:id st)])]
    (cond
      (nil? v) (t :rec/e-part)
      (< (long (:length v 0)) (long (:length p))) (t :rec/e-part-small)
      (:mountpoint v) (t :rec/e-part-mounted (vol-target v) (:mountpoint v))
      :else nil)))

;; --- the wizard -----------------------------------------------------------------------

(defn- steps [s]
  (let [files? (= :files (:method s))]
    (concat
     [{:id :archive :label (t :rec/archive) :page archive-page :help (t :rec/h-archive)
       :leave (fn [st] (when-not (:archive @st) (t :rec/e-archive)))}
      {:id :method :label (t :rec/method) :page method-page}
      {:id :point :label (t :rec/point) :page point-page :help (t :rec/h-point)}
      (if files?
        {:id :what :label (t :rec/what) :page files-page
         :leave (fn [st] (when (empty? (:files @st)) (t :rec/e-files)))}
        {:id :what :label (t :rec/what) :page what-page :help (t :rec/h-what)
         :leave (fn [st] (when (empty? (:sel @st)) (t :rec/e-items)))})]
     (if files?
       [{:id :dest :label (t :rec/dest) :page files-dest-page
         :leave (fn [st] (let [d (str/trim (str (:file-dest @st)))]
                           (when-not (.isDirectory (java.io.File. d)) (t :rec/e-folder))))}]
       (for [st (dest-steps s)]
         (assoc st
                :page (case (:kind st) :disk #(disk-page % st) :part #(part-page % st) :head #(head-page % st))
                :leave (case (:kind st)
                         :disk #(leave-disk % st)
                         :part #(leave-part % st)
                         :head (fn [x] (when-not (get-in @x [:targets (:id st)]) (t :rec/e-head)))))))
     [{:id :finish :label (t :wiz/finish) :final true :help (t :rec/h-finish)
       :page (fn [st] (w/summary-page (try (plan @st)
                                         (catch Throwable e [{:title (t :c/error (ex-message e))}]))))}
      {:id :options :label (t :wiz/options) :optional true :final true :page options-page}])))

(defn start!
  "Opens the recovery wizard. method: :disks or :files."
  [^Window owner app method]
  (let [state (atom {:app app :method method :sel #{} :targets {} :keep-sig true
                     :files #{} :file-dest (if (linux?) "/mnt" (System/getProperty "user.home"))})]
    (wizard/show! owner {:title (t :rec/title) :state state :steps steps
                         :on-proceed
                         (fn [s]
                           (progress/run-ops! owner app
                                              {:title (t :rec/running)
                                               :ops (plan s) :shutdown (:shutdown s)
                                               :success (t :rec/success)}))})))
