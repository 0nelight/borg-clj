(ns borgclj.gui.backup
  "The backup wizard, on the same pattern as recovery:

     Source -> Destination -> Finish (optional: Options)

   Disks are backed up with image (only allocated blocks, free space as zeros),
   files and folders with create."
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
  (:import (java.awt BorderLayout Component Dimension FlowLayout Window)
           (java.awt.event ActionListener)
           (java.io File)
           (java.time LocalTime)
           (java.time.format DateTimeFormatter)
           (javax.swing ButtonGroup JCheckBox JComboBox JComponent JLabel JPanel JRadioButton
                        JTextField JTree SwingUtilities)
           (javax.swing.event TreeExpansionEvent TreeWillExpandListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreeCellRenderer)))

(set! *warn-on-reflection* true)

(defn- cfg [s] @(:config (:app s)))
(defn- linux? [] (str/starts-with? (str (System/getProperty "os.name")) "Linux"))

;; --- source: disks -------------------------------------------------------------------

(defn- used ^String [v]
  (let [total (:total-bytes v) free (:free-bytes v)]
    (if (and total free) (log/human (- (long total) (long free))) "")))

(defn- source-rows [s]
  (let [d (pages/disks (:app s) false)
        vols (into {} (map (juxt :device identity)) (:volumes d))
        sel (:sources s)]
    (vec
     (concat
      (mapcat
       (fn [i dk]
         (let [tok (or (:token dk) (str "disk:" (:disk dk)))]
           (cons {:group true :token tok :checked (contains? sel tok)
                  :name (str (t :c/disk-n (inc (long i))) " (" (or (:name dk) tok) ")"
                             (when-let [m (:model dk)] (str " – " m)))
                  :cap (log/human (long (:length dk 0)))}
                 (for [p (:partitions dk) :when (:token p)
                       :let [v (vols (:device p))]]
                   {:token (:token p) :disk tok
                    :checked (or (contains? sel (:token p)) (contains? sel tok))
                    :disabled (contains? sel tok)
                    :name (str (or (:label p) (:name p)) " (" (:name p) ")")
                    :cap (log/human (long (:length p 0)))
                    :used (if v (used v) "")
                    :type (or (:fstype p) "")}))))
       (range) (:disks d))
      ;; Windows: volumes do not hang below their disks
      (let [under (set (keep :device (mapcat :partitions (:disks d))))
            loose (remove #(or (:error %) (contains? under (:device %))) (:volumes d))]
        (when (and (seq loose) (not (linux?)))
          (cons {:group true :name (t :bak/volumes) :header-only true}
                (for [v loose :let [tok (or (:token v) (str (:letter v) ":"))]]
                  {:token tok :checked (contains? sel tok)
                   :name (str (or (:label v) "") " (" (:letter v) ":)")
                   :cap (log/human (long (:length v 0)))
                   :used "" :type (or (:fstype v) (when (:ntfs v) "NTFS") "")}))))))))

(defn- disks-page [state]
  (w/async
   #(pages/disks (:app @state) true)
   (fn [_]
     (let [rows (atom (source-rows @state))
           tbl (w/row-table
                [{:title "" :check true :width 28 :get #(boolean (:checked %))}
                 {:title (t :c/partition) :width 300 :get :name
                  :icon #(when-not (:header-only %) (look/disk-icon))}
                 {:title (t :c/capacity) :width 90 :align :right :get #(:cap % "")}
                 {:title (t :c/used) :width 100 :align :right :get #(:used % "")}
                 {:title (t :c/type) :width 80 :get #(:type % "")}]
                rows
                {:on-toggle (fn [r on]
                              (when-not (:header-only r)
                                (swap! state update :sources
                                       (fn [sel]
                                         (let [sel (or sel #{})]
                                           (cond
                                             (not on) (disj sel (:token r))
                                             ;; the whole disk takes its partitions with it
                                             (:group r) (conj (into #{} (remove #(str/starts-with? % (:token r))) sel)
                                                              (:token r))
                                             :else (conj sel (:token r))))))
                                (reset! rows (source-rows @state))))})]
       (w/page (t :bak/disks-head)
               (w/framed (t :bak/disks-parts) tbl))))))

;; --- source: files --------------------------------------------------------------------

(defn- fs-node ^DefaultMutableTreeNode [^File f]
  (let [n (DefaultMutableTreeNode. f)]
    (when (.isDirectory f) (.add n (DefaultMutableTreeNode. ::placeholder)))
    n))

(defn- files-page [state]
  (let [root (DefaultMutableTreeNode. (t :bak/computer))
        model (DefaultTreeModel. root)
        tree (JTree. model)
        check (JCheckBox.)
        label (JLabel.)
        cell (JPanel. (FlowLayout. FlowLayout/LEFT 2 0))]
    (doseq [r (File/listRoots)] (.add root (fs-node r)))
    (.reload model)
    (.setOpaque cell false) (.setOpaque check false)
    (.add cell check) (.add cell label)
    (.setCellRenderer tree
                      (reify TreeCellRenderer
                        (getTreeCellRendererComponent [_ _ v _ _ _ _ _]
                          (let [o (.getUserObject ^DefaultMutableTreeNode v)]
                            (if (instance? File o)
                              (let [^File f o]
                                (.setVisible check true)
                                (.setSelected check (contains? (:paths @state) (str f)))
                                (.setText label (if (str/blank? (.getName f)) (str f) (.getName f)))
                                (.setIcon label (if (.isDirectory f) look/folder-icon look/file-icon)))
                              (do (.setVisible check false) (.setText label (str o)) (.setIcon label nil)))
                            cell))))
    (.addTreeWillExpandListener
     tree
     (reify TreeWillExpandListener
       (treeWillCollapse [_ _])
       (treeWillExpand [_ e]
         (let [^DefaultMutableTreeNode n (.getLastPathComponent (.getPath ^TreeExpansionEvent e))
               o (.getUserObject n)]
           (when (and (instance? File o) (= 1 (.getChildCount n))
                      (= ::placeholder (.getUserObject ^DefaultMutableTreeNode (.getChildAt n 0))))
             (.removeAllChildren n)
             (doseq [^File c (sort-by (fn [^File f] [(not (.isDirectory f)) (str/lower-case (.getName f))])
                                      (or (seq (.listFiles ^File o)) []))]
               (.add n (fs-node c)))
             (.nodeStructureChanged model n))))))
    (.addMouseListener
     tree
     (proxy [java.awt.event.MouseAdapter] []
       (mouseClicked [^java.awt.event.MouseEvent e]
         (when-let [tp (.getPathForLocation tree (.getX e) (.getY e))]
           (let [b (.getPathBounds tree tp)
                 o (.getUserObject ^DefaultMutableTreeNode (.getLastPathComponent tp))]
             (when (and (instance? File o) (< (.getX e) (+ (.x b) 22)))
               (swap! state update :paths
                      (fn [ps] (let [ps (or ps #{}) k (str o)] (if (contains? ps k) (disj ps k) (conj ps k)))))
               (.repaint tree)))))))
    (.setRootVisible tree false)
    (.setShowsRootHandles tree true)
    (.setRowHeight tree 22)
    (w/page (t :bak/files-head)
            (w/framed (t :c/files-folders) tree))))

;; --- destination --------------------------------------------------------------------

(defn- with-time
  "The archive name with the time of day appended -- for when today's name is
   already taken."
  ^String [^String nm]
  (str nm "-" (.format (LocalTime/now) (DateTimeFormatter/ofPattern "HHmm"))))

(defn- dest-page [state]
  (let [path (JTextField. (str (:repo (cfg @state))))
        name-field (JTextField. (str (:archive-name @state)) 30)
        series (JLabel. " ")
        init (JCheckBox. (t :bak/init) (boolean (:init (cfg @state))))
        g (ButtonGroup.)
        fresh (JRadioButton. (t :bak/new-archive) true)
        info! (fn []
                (.setText series (t :bak/reading))
                (let [c (cfg @state)]
                  (doto (Thread.
                         ^Runnable
                         (fn []
                           (let [msg (try (let [as (data/archives c)
                                                taken (set (map :name as))
                                                nm (str/trim (.getText name-field))]
                                            ;; today's name taken: append the time instead of
                                            ;; showing an error only on Next
                                            (when (taken nm)
                                              (let [alt (with-time nm)]
                                                (SwingUtilities/invokeLater #(.setText name-field alt))))
                                            (if (empty? as)
                                              (t :bak/empty)
                                              (t :bak/count (count as) (:name (last as)))))
                                          (catch Throwable e (t :bak/unreachable (ex-message e))))]
                             (SwingUtilities/invokeLater #(.setText series msg)))))
                    (.setDaemon true) (.start))))
        browse (w/button (t :c/browse)
                         (fn []
                           (when (connect/edit! (SwingUtilities/getWindowAncestor path) (:config (:app @state)))
                             (.setText path (str (:repo (cfg @state))))
                             (info!))))
        row (fn [^String l ^JComponent c & [^JComponent right]]
              (let [p (doto (JPanel. (BorderLayout. 6 0)) (.setOpaque false) (look/pad! 8 24 0 0)
                            (.add (JLabel. l) BorderLayout/WEST)
                            (.add c BorderLayout/CENTER))]
                (when right (.add p right BorderLayout/EAST))
                p))]
    (.add g fresh)
    (doseq [^JComponent c [fresh init]] (.setOpaque c false))
    (.addActionListener path (reify ActionListener
                               (actionPerformed [_ _]
                                 (swap! (:config (:app @state)) assoc :repo (str/trim (.getText path)))
                                 (info!))))
    (.addCaretListener name-field (reify javax.swing.event.CaretListener
                                    (caretUpdate [_ _] (swap! state assoc :archive-name (.getText name-field)))))
    (.addActionListener init (reify ActionListener
                               (actionPerformed [_ _]
                                 (swap! (:config (:app @state)) assoc :init (.isSelected init)))))
    (info!)
    (w/page (t :bak/dest-head)
            (doto (JPanel. (BorderLayout.))
              (.setOpaque false)
              (.add (w/vbox fresh
                            (row (t :bak/repository) path browse)
                            (row (t :bak/archive-name) name-field)
                            (doto init (look/pad! 8 20 0 0))
                            (doto series (look/pad! 12 24 0 0) (.setForeground (look/grey-text))))
                    BorderLayout/NORTH)))))

;; --- summary and options ----------------------------------------------------------

(defn plan
  "The operations \"Proceed\" starts, built from the same state the summary shows."
  [s]
  (let [c (merge (cfg s) (select-keys s [:compression :block]))
        a (str/trim (str (:archive-name s)))]
    (if (= :files (:method s))
      [{:title (t :bak/op-files a)
        :kind :create
        :argv (-> ["create" (config/location c a)]
                  (into (sort (:paths s)))
                  (cond-> (seq (:compression c)) (into ["--compression" (:compression c)])
                          (:init c) (conj "--init"))
                  (into (config/connection-args c)))
        :lines (concat [[(t :rec/k-dest) (config/location c a)]]
                       (for [p (sort (:paths s))] [(t :rec/k-source) p]))}]
      [{:title (t (if (:dry-run s) :bak/op-dry :bak/op-disks) a)
        :kind :image
        :argv (config/image-args c {:archive a :sources (vec (sort (:sources s)))
                                    :dry-run (:dry-run s) :raw (:raw s)})
        :lines (concat [[(t :rec/k-dest) (config/location c a)]
                        [(t :bak/k-compression) (:compression c)]
                        [(t :bak/k-block) (:block c)]]
                       (for [src (sort (:sources s))] [(t :rec/k-source) src]))}])))

(defn- options-page [state]
  (let [files? (= :files (:method @state))
        comp (doto (JComboBox. (object-array ["none" "lz4" "zstd,1" "zstd,3" "zstd,9"]))
               (.setSelectedItem (or (:compression @state) (:compression (cfg @state)))))
        block (JTextField. (str (or (:block @state) (:block (cfg @state)))) 8)
        cb (fn [^String s k]
             (let [c (JCheckBox. s (boolean (k @state)))]
               (.setOpaque c false)
               (.addActionListener c (reify ActionListener
                                       (actionPerformed [_ _] (swap! state assoc k (.isSelected c)))))
               c))
        row (fn [^String l ^Component c]
              (doto (JPanel. (FlowLayout. FlowLayout/LEFT 6 2)) (.setOpaque false)
                    (.add (doto (JLabel. l) (.setPreferredSize (Dimension. 120 20)))) (.add c)))]
    (.addActionListener comp (reify ActionListener
                               (actionPerformed [_ _] (swap! state assoc :compression (str (.getSelectedItem comp))))))
    (.addCaretListener block (reify javax.swing.event.CaretListener
                               (caretUpdate [_ _] (swap! state assoc :block (str/trim (.getText block))))))
    (w/page (t :bak/options)
            (doto (JPanel. (BorderLayout.))
              (.setOpaque false)
              (.add (w/vbox (row (t :bak/compression) comp)
                            (when-not files? (row (t :bak/block) block))
                            (when-not files? (cb (t :bak/raw) :raw))
                            (when-not files? (cb (t :bak/dry-run) :dry-run))
                            (cb (t :bak/shutdown) :shutdown))
                    BorderLayout/NORTH)))))

(defn- steps [s]
  [(if (= :files (:method s))
     {:id :source :label (t :bak/source) :page files-page
      :leave (fn [st] (when (empty? (:paths @st)) (t :bak/e-files)))}
     {:id :source :label (t :bak/source) :page disks-page
      :leave (fn [st] (when (empty? (:sources @st)) (t :bak/e-disks)))})
   {:id :dest :label (t :bak/dest) :page dest-page
    :leave (fn [st] (let [c (cfg @st)
                          nm (str/trim (str (:archive-name @st)))]
                      (cond (str/blank? (str (:repo c))) (t :bak/e-repo)
                            (str/blank? nm) (t :bak/e-name)
                            (some #(= nm (:name %)) (try (data/archives c) (catch Throwable _ nil)))
                            (t :bak/e-taken nm)
                            :else (do (try (config/save! c) (catch Throwable _ nil)) nil))))}
   {:id :finish :label (t :wiz/finish) :final true
    :page (fn [st] (w/summary-page (try (plan @st)
                                        (catch Throwable e [{:title (t :c/error (ex-message e))}]))))}
   {:id :options :label (t :wiz/options) :optional true :final true :page options-page}])

(defn start!
  "Opens the backup wizard. method: :disks or :files."
  [^Window owner app method]
  (let [state (atom {:app app :method method :sources #{} :paths #{}
                     :archive-name (config/archive-name (:archive @(:config app)))})]
    (wizard/show! owner {:title (t :bak/title) :state state :steps steps
                         :on-proceed
                         (fn [s]
                           (progress/run-ops! owner app
                                              {:title (t :bak/running) :ops (plan s) :shutdown (:shutdown s)
                                               :success (t :bak/success)}))})))
