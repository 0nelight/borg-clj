(ns borgclj.gui.widgets
  "Tables, calendar, disk map and the loading panel of the wizards."
  (:require [borgclj.gui.i18n :refer [t]]
            [borgclj.gui.look :as look]
            [borgclj.log :as log]
            [clojure.string :as str])
  (:import (java.awt BorderLayout Color Component Cursor Dimension FlowLayout Font
                     GradientPaint Graphics Graphics2D GridLayout RenderingHints)
           (java.awt.event MouseAdapter MouseEvent)
           (java.time DayOfWeek LocalDate YearMonth)
           (java.time.format TextStyle)
           (java.util Locale)
           (javax.swing BorderFactory JComponent JLabel JPanel JScrollPane JTable
                        ListSelectionModel SwingConstants SwingUtilities)
           (javax.swing.table AbstractTableModel DefaultTableCellRenderer TableCellRenderer)))

(set! *warn-on-reflection* true)

(defn async
  "A panel that says \"Loading …\", runs load on a thread of its own and then
   puts (build result) in its place; if load fails, the message stands there.
   A repository over ssh takes seconds, and the window must not freeze meanwhile."
  ^JPanel [load build]
  (let [p (JPanel. (BorderLayout.))]
    (.setBackground p Color/WHITE)
    (.add p (look/centered (t :c/loading)) BorderLayout/CENTER)
    (doto (Thread.
           ^Runnable
           (fn []
             (let [r (try {:ok (load)} (catch Throwable t {:error t}))]
               (SwingUtilities/invokeLater
                (fn []
                  (.removeAll p)
                  (if-let [t (:error r)]
                    (.add p (doto (JLabel. (str "<html><b>" (t :c/failed) "</b><br><br>"
                                                (or (ex-message ^Throwable t) (str t))
                                                "</html>"))
                              (.setForeground (look/rgb 0xb00020))
                              (.setVerticalAlignment SwingConstants/TOP)
                              (look/pad! 12 12 12 12))
                          BorderLayout/CENTER)
                    (.add p ^Component (build (:ok r)) BorderLayout/CENTER))
                  (.revalidate p)
                  (.repaint p))))))
      (.setDaemon true)
      (.start))
    p))

;; --- row table -------------------------------------------------------------------

(defn row-table
  "A table over a vector of row maps. cols: [{:title :get :width :align :check
   :icon}]. Rows with :group are headings (bold, like \"Disk 1\" in Acronis);
   the rows below them are indented in the name column, as in a tree.
   on-select gets the selected row; on-toggle gets the row and the new value
   when a checkbox column (:check) is clicked."
  ^JTable [cols rows-atom {:keys [on-select on-toggle]}]
  (let [model (proxy [AbstractTableModel] []
                (getRowCount [] (count @rows-atom))
                (getColumnCount [] (count cols))
                (getColumnName [c] (:title (nth cols c)))
                (getColumnClass [c] (if (:check (nth cols c)) Boolean Object))
                (isCellEditable [r c] (boolean (and (:check (nth cols c))
                                                    (not (:disabled (nth @rows-atom r))))))
                (getValueAt [r c] ((:get (nth cols c)) (nth @rows-atom r)))
                (setValueAt [v r c]
                  (when on-toggle (on-toggle (nth @rows-atom r) (boolean v)))
                  (.fireTableDataChanged ^AbstractTableModel this)))
        grouped? (reify clojure.lang.IDeref (deref [_] (boolean (some :group @rows-atom))))
        table (JTable. ^AbstractTableModel model)
        base (DefaultTableCellRenderer.)
        render (reify TableCellRenderer
                 (getTableCellRendererComponent [_ tbl v sel focus r c]
                   (let [row (nth @rows-atom r)
                         col (nth cols c)
                         ^JLabel l (.getTableCellRendererComponent base tbl v sel false r c)
                         main (first (keep-indexed (fn [i x] (when-not (:check x) i)) cols))]
                     ;; tree indentation: rows below a group row move right
                     (.setBorder l (BorderFactory/createEmptyBorder
                                    0 (if (and (= c main) (not (:group row)) @grouped?) 20 2) 0 2))
                     (.setIcon l (when-let [f (:icon col)] (f row)))
                     (.setHorizontalAlignment l (if (= :right (:align col))
                                                  SwingConstants/RIGHT SwingConstants/LEFT))
                     (.setFont l (cond (:group row) (look/font 13 Font/BOLD)
                                       :else (look/font 13)))
                     (.setForeground l (cond sel Color/WHITE
                                             (:disabled row) (look/grey-text)
                                             (:group row) (look/title-blue)
                                             :else Color/BLACK))
                     l)))]
    (.setSelectionModel table (doto (javax.swing.DefaultListSelectionModel.)
                            (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)))
    (doseq [[i col] (map-indexed vector cols)]
      (let [tc (.getColumn (.getColumnModel table) (int i))]
        (when-not (:check col) (.setCellRenderer tc render))
        (when-let [w (:width col)]
          (.setPreferredWidth tc (int w))
          (when (:check col) (.setMaxWidth tc (int w))))))
    (doto table
      (.setRowHeight 22)
      (.setShowVerticalLines false)
      (.setGridColor (look/rgb 0xe8ecf0))
      (.setFillsViewportHeight true)
      (.setSelectionBackground (look/select-blue)))
    (when on-select
      (.addListSelectionListener
       (.getSelectionModel table)
       (reify javax.swing.event.ListSelectionListener
         (valueChanged [_ e]
           (when-not (.getValueIsAdjusting ^javax.swing.event.ListSelectionEvent e)
             (let [r (.getSelectedRow table)]
               (when (<= 0 r (dec (count @rows-atom)))
                 (on-select (nth @rows-atom r)))))))))
    table))

(defn select-row! [^JTable t pred rows]
  (when-let [i (first (keep-indexed (fn [i r] (when (pred r) i)) rows))]
    (.setRowSelectionInterval t (int i) (int i))
    (.scrollRectToVisible t (.getCellRect t (int i) 0 true))))

(defn text
  "A paragraph that wraps instead of being cut off."
  ^JLabel [^String s]
  (JLabel. (str "<html><div style='width:480px'>" s "</div></html>")))

(defn button ^javax.swing.JButton [^String s f]
  (doto (javax.swing.JButton. s)
    (.addActionListener (reify java.awt.event.ActionListener (actionPerformed [_ _] (f))))))

(defn vbox ^JPanel [& cs]
  (let [p (JPanel.)]
    (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))
    (.setOpaque p false)
    (doseq [^JComponent c cs :when c] (.setAlignmentX c 0.0) (.add p c))
    p))

(defn framed
  "A table below the dark blue toolbar, like \"Details\" in Acronis. left is a
   text or already a component (such as a clickable \"Disk properties\");
   right is optional."
  ^JPanel [left ^JComponent table & [right]]
  (let [b (look/bar)
        p (JPanel. (BorderLayout.))]
    (let [^Component l (if (string? left) (look/white-label left) left)]
      (.add b l BorderLayout/WEST))
    (when right (.add b ^Component right BorderLayout/EAST))
    (doto p
      (.setBorder (BorderFactory/createLineBorder (look/line)))
      (.add b BorderLayout/NORTH)
      (.add (doto (JScrollPane. table)
              (.setBorder (BorderFactory/createEmptyBorder)))
            BorderLayout/CENTER))))

(defn page
  "A wizard page: heading, body below."
  ^JPanel [^String title ^JComponent body]
  (doto (JPanel. (BorderLayout.))
    (.setBackground Color/WHITE)
    (.add (look/heading title) BorderLayout/NORTH)
    (.add (doto (JPanel. (BorderLayout.))
            (.setOpaque false)
            (.setBorder (BorderFactory/createEmptyBorder 0 10 10 10))
            (.add body BorderLayout/CENTER))
          BorderLayout/CENTER)))

(defn summary-html
  "The summary page as Acronis shows it: \"Operations\", their number, then each
   operation with its details indented."
  ^String [ops]
  (let [esc #(-> (str %) (str/replace "&" "&amp;") (str/replace "<" "&lt;"))]
    (str "<html><body style='font-family:sans-serif;font-size:11px'>"
         "<b>" (t :wiz/operations) "</b><br><br><i>" (t :wiz/op-count (count ops)) "</i><br><br>"
         (apply str
                (map-indexed
                 (fn [i op]
                   (str "<b>" (inc i) ". " (esc (:title op)) "</b>"
                        "<table cellspacing=0 cellpadding=1 style='margin-left:18px'>"
                        (apply str (for [[k v] (:lines op)]
                                     (str "<tr><td width=150><i>" (esc k) ":</i></td><td><i>"
                                          (esc v) "</i></td></tr>")))
                        "</table><br>"))
                 ops))
         "</body></html>")))

(defn summary-page [ops]
  (page (t :wiz/summary)
          (doto (JScrollPane. (doto (javax.swing.JEditorPane. "text/html" (summary-html ops))
                                (.setEditable false)
                                (.setBackground java.awt.Color/WHITE)))
            (.setBorder (BorderFactory/createLineBorder (look/line))))))

;; --- calendar --------------------------------------------------------------------

(defn calendar
  "A month sheet as in Acronis: days with a backup on green, the chosen one
   framed. marked is a set of LocalDate; on-pick gets the clicked day."
  ^JPanel [marked ^LocalDate chosen on-pick]
  (let [month (atom (YearMonth/from (or chosen (LocalDate/now))))
        outer (JPanel. (BorderLayout.))
        grid (JPanel. (GridLayout. 0 7 2 2))
        title (JLabel. "" SwingConstants/CENTER)
        de (Locale/getDefault)
        fill! (atom nil)
        arrow (fn ^Component [^String s d]
                (look/clickable! (doto (JLabel. s) (.setForeground (look/title-blue))
                                       (.setFont (look/font 13 Font/BOLD)))
                                 (fn [] (swap! month #(.plusMonths ^YearMonth % (long d))) (@fill!))))]
    (reset! fill!
            (fn []
              (let [^YearMonth ym @month
                    first-day (.atDay ym 1)
                    lead (dec (.getValue (.getDayOfWeek first-day)))]
                (.setText title (str (.getDisplayName (.getMonth ym) TextStyle/FULL_STANDALONE de)
                                     " " (.getYear ym)))
                (.removeAll grid)
                (doseq [d [DayOfWeek/MONDAY DayOfWeek/TUESDAY DayOfWeek/WEDNESDAY
                           DayOfWeek/THURSDAY DayOfWeek/FRIDAY DayOfWeek/SATURDAY
                           DayOfWeek/SUNDAY]]
                  (.add grid (doto (JLabel. (.getDisplayName ^DayOfWeek d TextStyle/SHORT de)
                                            SwingConstants/CENTER)
                               (.setForeground (if (#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} d)
                                                 (look/rgb 0xc0392b) (look/title-blue)))
                               (.setFont (look/font 12)))))
                (doseq [i (range 42)]
                  (let [day (.plusDays first-day (long (- i lead)))
                        in? (= (.getMonth day) (.getMonth ym))
                        l (JLabel. (str (.getDayOfMonth day)) SwingConstants/CENTER)]
                    (.setFont l (if (= day (LocalDate/now)) (look/font 12 Font/BOLD) (look/font 12)))
                    (.setForeground l (if in? Color/BLACK (look/grey-text)))
                    (when (contains? marked day)
                      (.setOpaque l true)
                      (.setBackground l (look/rgb 0x3aa655))
                      (.setForeground l Color/WHITE)
                      (look/clickable! l (fn [] (on-pick day))))
                    (when (= day chosen)
                      (.setBorder l (BorderFactory/createLineBorder (look/rgb 0x1d4f91) 2)))
                    (.add grid l)))
                (.revalidate grid) (.repaint grid))))
    (@fill!)
    (doto outer
      (.setBackground Color/WHITE)
      (.setBorder (BorderFactory/createLineBorder (look/line)))
      (.add (doto (JPanel. (BorderLayout.))
              (.setOpaque false)
              (.add ^Component (arrow " ◀ " -1) BorderLayout/WEST)
              (.add title BorderLayout/CENTER)
              (.add ^Component (arrow " ▶ " 1) BorderLayout/EAST))
            BorderLayout/NORTH)
      (.add (doto grid (.setOpaque false) (look/pad! 4 4 4 4)) BorderLayout/CENTER)
      (.setPreferredSize (Dimension. 260 210)))))

;; --- disk map --------------------------------------------------------------------

(defn disk-map
  "The bar below the destination table: the partitions of a disk to scale,
   size and disk icon on the left. parts: [{:offset :length :label :letter :fs}]."
  ^JComponent [length parts]
  (let [len (max 1 (long (or length 1)))]
    (doto (proxy [JComponent] []
            (paintComponent [^Graphics g]
              (let [^JComponent this this
                    g2 ^Graphics2D (.create g)
                    w (.getWidth this) h (.getHeight this)
                    x0 56 bw (- w x0 6)]
                (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                (.setRenderingHint g2 RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
                (.setColor g2 (look/rgb 0xf4f6f8))
                (.fillRoundRect g2 0 0 (dec w) (dec h) 8 8)
                (.setColor g2 (look/line))
                (.drawRoundRect g2 0 0 (dec w) (dec h) 8 8)
                (.paintIcon ^javax.swing.Icon (look/disk-icon 24) this g2 14 6)
                (.setColor g2 Color/BLACK)
                (.setFont g2 (look/font 11))
                (.drawString g2 ^String (log/human len) (int 8) (int (- h 10)))
                (if (empty? parts)
                  (do (.setColor g2 (look/rgb 0xd9dde2))
                      (.fillRect g2 x0 8 bw (- h 16))
                      (.setColor g2 (look/rgb 0x555555))
                      (.drawString g2 (t :c/unallocated-short) (int (+ x0 8)) (int (/ h 2))))
                  (doseq [p parts]
                    (let [x (+ x0 (long (* bw (/ (double (:offset p 0)) len))))
                          pw (max 30 (long (* bw (/ (double (:length p)) len))))]
                      (.setPaint g2 (GradientPaint. 0 0 (look/rgb 0x9be07f) 0 12 (look/rgb 0x4caf36)))
                      (.fillRect g2 (int x) 8 (int (- pw 2)) 12)
                      (.setColor g2 (look/rgb 0x2c7a1e))
                      (.drawRect g2 (int x) 8 (int (- pw 2)) (- h 16))
                      (.setColor g2 Color/BLACK)
                      (let [clip (.getClip g2)]
                        (.clipRect g2 (int x) 0 (int (- pw 4)) h)
                        (.drawString g2 (str (or (:label p) "") (when-let [l (:letter p)] (str " (" l ":)")))
                                     (int (+ x 4)) (int 34))
                        (.drawString g2 (str (log/human (long (:length p))) "  " (or (:fs p) ""))
                                     (int (+ x 4)) (int 48))
                        (.setClip g2 clip)))))
                (.dispose g2))))
      (.setPreferredSize (Dimension. 400 58)))))

(defn legend ^JPanel []
  (let [sw (fn ^JLabel [^Color c ^String s]
             (doto (JLabel. s)
               (.setIcon (reify javax.swing.Icon
                           (getIconWidth [_] 12) (getIconHeight [_] 14)
                           (paintIcon [_ _ g x y]
                             (.setColor ^Graphics g c) (.fillRect ^Graphics g x y 10 13)
                             (.setColor ^Graphics g Color/GRAY) (.drawRect ^Graphics g x y 10 13))))
               (.setFont (look/font 12))))]
    (doto (JPanel. (FlowLayout. FlowLayout/LEFT 14 2))
      (.setOpaque false)
      (.add ^Component (sw (look/rgb 0x6cc24f) (t :c/primary)))
      (.add ^Component (sw (look/rgb 0xd9dde2) (t :c/unallocated))))))
