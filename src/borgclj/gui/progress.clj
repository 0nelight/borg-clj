(ns borgclj.gui.progress
  "The progress window, and what it runs: a sequence of operations, each one a
   borgi verb, one after the other in the same process -- through the same job
   mechanism as the web interface, so progress comes straight from the run.

   As in Acronis: the operation on top, its bar below, the total progress, the
   time left, the \"Shut down the computer\" tick box and Cancel. A message at
   the end."
  (:require [borgclj.gui.i18n :refer [t]]
            [borgclj.gui.look :as look]
            [borgclj.log :as log]
            [borgclj.ui.html :as h]
            [borgclj.ui.job :as job]
            [borgclj.ui.pages :as pages]
            [clojure.string :as str])
  (:import (java.awt BorderLayout Dialog$ModalityType Dimension FlowLayout Font
                     GridBagConstraints GridBagLayout Insets Window)
           (java.awt.event ActionListener)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (javax.swing BorderFactory JButton JCheckBox JDialog JLabel JOptionPane
                        JPanel JProgressBar JScrollPane JTextArea Timer WindowConstants)))

(set! *warn-on-reflection* true)

(defn- stop-path [kind]
  (when (= :image kind)
    (.resolve (Files/createTempDirectory "borgi-gui" (make-array FileAttribute 0)) "stop")))

(defn- shutdown! []
  (let [cmd (if (str/starts-with? (str (System/getProperty "os.name")) "Windows")
              ["shutdown" "/s" "/t" "0"]
              ["systemctl" "poweroff"])]
    (.start (ProcessBuilder. ^java.util.List cmd))))

(defn- grid-add [^JPanel p c row & {:keys [fill] :or {fill true}}]
  (.add p ^java.awt.Component c
        (GridBagConstraints. 0 (int row) 1 1 1.0 0.0 GridBagConstraints/WEST
                             (if fill GridBagConstraints/HORIZONTAL GridBagConstraints/NONE)
                             (Insets. 4 14 4 14) 0 0)))

(defn- numbers
  "One line on what the running verb is doing, from its progress map."
  ^String [s]
  (let [{:keys [phase] :as p} (:progress s)
        hb (fn [k] (log/human (long (get p k 0))))]
    (case phase
      :copy (str/join " · " [(str (:source p)) (t :pn/read (hb :read)) (t :pn/zeroed (hb :zeroed))
                             (t :pn/sent (hb :sent)) (h/rate (:read-per-second p))])
      :restore (str (t :pn/written (hb :done) (hb :of)) " · " (h/rate (:per-second p)))
      :mft-records (t :pn/mft-records (:records p))
      :mft-dirs (t :pn/mft-dirs (:dirs p) (:dirs-total p) (:files p))
      :start (t :pn/start)
      :snapshot (t :pn/snapshot (name (:provider p)))
      :scanning (t :pn/scanning)
      :connected (t :pn/connected)
      :done (t :pn/done (hb :size) (hb :read) (hb :sent))
      :stopped (t :pn/stopped (:block p) (:of p))
      :failed (:error p)
      nil)))

(defn run-ops!
  "Runs ops: [{:title \"...\" :kind :restore|:image|:get|... :argv [...]
   :after (fn [] ...)}]. Kehrt zurueck, wenn das Fenster geschlossen ist.
   Liefert :done, :failed oder :cancelled."
  [^Window owner app {:keys [title ops success shutdown]}]
  (let [dlg (JDialog. owner ^String title Dialog$ModalityType/APPLICATION_MODAL)
        n (count ops)
        what (JLabel. " ")
        numbers (JLabel. " ")
        eta (JLabel. " ")
        bar (doto (JProgressBar. 0 1000) (.setStringPainted true))
        total (doto (JProgressBar. 0 1000) (.setStringPainted true))
        off (JCheckBox. (t :prog/shutdown) (boolean shutdown))
        cancel (JButton. (t :wiz/cancel))
        i (atom -1)
        outcome (atom :done)
        timer (atom nil)
        history (:history app)
        record! (fn [s]
                  (let [[lines _] (job/log-from s 0)]
                    (swap! history conj {:time (java.time.LocalDateTime/now)
                                         :title (:title s) :state (:state s)
                                         :argv (:argv s) :log lines})))
        start-next! (fn []
                      (let [op (nth ops (swap! i inc))]
                        (.setText what (t :prog/step (inc @i) n (:title op)))
                        (job/start! (:job app) {:run (:run app) :kind (:kind op)
                                                :title (:title op) :argv (:argv op)
                                                :stop (stop-path (:kind op))})))
        finish! (fn [state s]
                  (.stop ^Timer @timer)
                  (reset! outcome state)
                  (.dispose dlg)
                  (case state
                    :done (do (JOptionPane/showMessageDialog
                               owner (or success (t :prog/ok))
                               ^String title JOptionPane/INFORMATION_MESSAGE)
                              (when (.isSelected off) (shutdown!)))
                    :cancelled (JOptionPane/showMessageDialog
                                owner (t :prog/cancelled) ^String title
                                JOptionPane/WARNING_MESSAGE)
                    :failed (let [[lines _] (job/log-from s 0)
                                  area (doto (JTextArea. ^String (str/join "\n" (take-last 30 lines)) 14 70)
                                         (.setEditable false)
                                         (.setFont (Font. Font/MONOSPACED Font/PLAIN 12)))]
                              (JOptionPane/showMessageDialog
                               owner (object-array [(t :prog/failed (last lines))
                                                    (JScrollPane. area)])
                               ^String title JOptionPane/ERROR_MESSAGE))))
        tick! (fn []
                (let [s (job/snapshot (:job app))
                      pct (pages/percent-of s)
                      done-before (/ (double @i) (max 1 n))]
                  (.setValue bar (int (* 10 (or pct 0.0))))
                  (.setString bar (if pct (format "%.0f %%" (double pct)) "…"))
                  (.setIndeterminate bar (and (= :running (:state s)) (nil? pct)))
                  (let [done-share (+ done-before (/ (or pct 0.0) 100.0 (max 1 n)))]
                    (.setValue total (int (* 1000 done-share)))
                    (.setString total (t :prog/total (* 100.0 done-share))))
                  (.setText numbers (t :prog/current (or (numbers s) (t :prog/preparing))))
                  (.setText eta (if-let [e (get-in s [:progress :eta-seconds])]
                                  (t :prog/left (h/duration e))
                                  " "))
                  (.setEnabled cancel (boolean (job/stoppable? s)))
                  (case (:state s)
                    :running nil
                    :done (do (record! s)
                              (when-let [f (:after (nth ops @i))] (f))
                              (if (< (inc @i) n) (start-next!) (finish! :done s)))
                    (:failed :cancelled) (do (record! s) (finish! (:state s) s))
                    nil)))
        p (JPanel. (GridBagLayout.))]
    (.setFont what (look/font 14 Font/BOLD))
    (grid-add p (look/heading (t :prog/title)) 0)
    (grid-add p what 1)
    (grid-add p bar 2)
    (grid-add p numbers 3)
    (grid-add p (JLabel. (t :prog/total-label)) 4)
    (grid-add p total 5)
    (grid-add p eta 6)
    (grid-add p off 7 :fill false)
    (.setBackground p java.awt.Color/WHITE)
    (.addActionListener cancel (reify ActionListener
                                 (actionPerformed [_ _] (job/stop! (:job app)))))
    (doto (.getContentPane dlg)
      (.setLayout (BorderLayout.))
      (.add p BorderLayout/CENTER)
      (.add (doto (JPanel. (FlowLayout. FlowLayout/RIGHT 8 8))
              (.setBackground (look/footer))
              (.setBorder (BorderFactory/createMatteBorder 1 0 0 0 (look/line)))
              (.add cancel))
            BorderLayout/SOUTH))
    (start-next!)
    (reset! timer (doto (Timer. 500 (reify ActionListener (actionPerformed [_ _] (tick!))))
                    (.setInitialDelay 200)))
    (.start ^Timer @timer)
    (doto dlg
      (.setDefaultCloseOperation WindowConstants/DO_NOTHING_ON_CLOSE)
      (.setSize (Dimension. 620 360))
      (.setLocationRelativeTo owner)
      (.setVisible true))
    @outcome))
