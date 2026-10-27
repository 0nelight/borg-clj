(ns borgclj.gui.wizard
  "The frame of every wizard, modelled on Acronis True Image:

     left     blue bar \"Required steps:\", the current step with a green arrow on
              a light band, finished ones ticked and clickable, the next one
              clickable, later ones grey; below \"Optional steps:\"
     top      round back button and the title
     right    the page of the step
     bottom   help on the left, \"Next >\" and \"Cancel\" on the right; on the
              last page \"Options\", \"Proceed\", \"Cancel\"

   Which steps exist depends on the answers (whole disk or single partitions,
   disks or files), so :steps is a function of the state and the bar is
   rebuilt on every change.

   A step is a map:
     :id        key
     :label     name in the bar
     :optional  listed under \"Optional steps\"
     :page      (fn [state] JComponent) -- builds the page; may change state
     :leave     (fn [state] nil | message | :stay) -- runs before \"Next\"; a
                message is shown, :stay means the step asked itself and the
                answer was no
     :final     a last page: \"Proceed\" instead of \"Next\"
     :help      one sentence for the help button"
  (:require [borgclj.gui.i18n :refer [t]]
            [borgclj.gui.look :as look])
  (:import (java.awt BorderLayout Color Component Dialog$ModalityType Dimension FlowLayout
                     Font Window)
           (java.awt.event ActionListener)
           (javax.swing BorderFactory Box BoxLayout JButton JComponent JDialog
                        JLabel JOptionPane JPanel SwingConstants WindowConstants)))

(set! *warn-on-reflection* true)

(defn- button ^JButton [^String s f]
  (doto (JButton. s)
    (.setPreferredSize (Dimension. 104 28))
    (.addActionListener (reify ActionListener (actionPerformed [_ _] (f))))))

(defn- step-row
  "One line of the step bar."
  ^JComponent [{:keys [label]} kind go]
  (let [l (JLabel. ^String label)
        row (JPanel. (BorderLayout. 6 0))]
    (.setOpaque row (= kind :current))
    (.setBackground row (Color. 255 255 255 235))
    (.setBorder row (BorderFactory/createEmptyBorder 3 8 3 6))
    (.setFont l (look/font 12 Font/ITALIC))
    (case kind
      :current (do (.setForeground l (look/rgb 0x1a1a1a)) (.setIcon l look/arrow-icon))
      :done (do (.setForeground l Color/WHITE) (.setIcon l look/check-icon)
                (.setText l (str "<html><u>" label "</u></html>"))
                (look/clickable! l go))
      :next (do (.setForeground l Color/WHITE) (.setIcon l look/blank-icon)
                (.setText l (str "<html><u>" label "</u></html>"))
                (look/clickable! l go))
      :later (do (.setForeground l (look/grey-text)) (.setIcon l look/blank-icon)))
    (.add row l BorderLayout/CENTER)
    (.setMaximumSize row (Dimension. 1000 26))
    (.setAlignmentX row 0.0)
    row))

(defn- side-caption ^JLabel [^String s]
  (doto (JLabel. s)
    (.setForeground Color/WHITE)
    (.setFont (look/font 13 Font/BOLD))
    (.setBorder (BorderFactory/createEmptyBorder 10 12 6 6))
    (.setAlignmentX 0.0)))

(defn show!
  "Opens the wizard as a window of its own above owner and returns once it is
   closed. on-proceed gets the state when \"Proceed\" is pressed on the last
   page, and runs after the window has closed."
  [^Window owner {:keys [title state steps on-proceed]}]
  (let [dlg (JDialog. owner ^String title Dialog$ModalityType/APPLICATION_MODAL)
        cur (atom 0)
        visited (atom #{0})
        proceed? (atom false)
        side (look/gradient-panel (look/side-top) (look/side-bottom) true)
        body (JPanel. (BorderLayout.))
        footer (JPanel. (BorderLayout.))
        right-buttons (JPanel. (FlowLayout. FlowLayout/RIGHT 8 8))
        back (JButton. look/back-icon)
        render! (atom nil)
        all-steps (fn [] (vec (steps @state)))
        go! (fn [i]
              (let [n (count (all-steps))]
                (when (< -1 i n)
                  (reset! cur i)
                  (swap! visited conj i)
                  (@render!))))
        leave-ok? (fn []
                    (let [st (nth (all-steps) @cur nil)
                          r (when-let [f (:leave st)] (f state))]
                      (cond
                        (nil? r) true
                        (= :stay r) false
                        :else (do (JOptionPane/showMessageDialog
                                   dlg (str r) ^String title JOptionPane/WARNING_MESSAGE)
                                  false))))
        next! (fn [] (when (leave-ok?) (go! (inc @cur))))
        jump! (fn [i]
                ;; Backwards always; forwards only across steps that pass their
                ;; checks -- otherwise the destination could be skipped.
                (if (<= i @cur)
                  (go! i)
                  (loop []
                    (cond
                      (>= @cur i) (go! i)
                      (leave-ok?) (do (swap! cur inc) (recur))
                      :else (go! @cur)))))
        proceed! (fn [] (when (leave-ok?) (reset! proceed? true) (.dispose dlg)))]
    (reset! render!
            (fn []
              (let [ss (all-steps)
                    i (min @cur (dec (count ss)))
                    st (nth ss i)
                    req (keep-indexed (fn [j s] (when-not (:optional s) [j s])) ss)
                    opt (keep-indexed (fn [j s] (when (:optional s) [j s])) ss)
                    kind (fn [j] (cond (= j i) :current
                                       (< j i) :done
                                       (or (= j (inc i)) (contains? @visited j)) :next
                                       :else :later))
                    top (JPanel.)
                    bottom (JPanel.)]
                (.removeAll side)
                (.setLayout side (BorderLayout.))
                (.setLayout top (BoxLayout. top BoxLayout/Y_AXIS))
                (.setOpaque top false)
                (.add top (side-caption (t :wiz/required)))
                (doseq [[j s] req] (.add top (step-row s (kind j) #(jump! j))))
                (.setLayout bottom (BoxLayout. bottom BoxLayout/Y_AXIS))
                (.setOpaque bottom false)
                (when (seq opt)
                  (.add bottom (side-caption (t :wiz/optional)))
                  ;; as in Acronis: grey until the summary has been reached
                  (let [open? (some (fn [[j s]] (and (:final s) (<= j (max i (apply max @visited))))) (map-indexed vector ss))]
                    (doseq [[j s] opt]
                      (.add bottom (step-row s (cond (= j i) :current open? :next :else :later) #(go! j))))))
                (.add bottom (Box/createVerticalStrut 18))
                (.add side top BorderLayout/NORTH)
                (.add side bottom BorderLayout/SOUTH)
                (.removeAll body)
                (.add body ^JComponent ((:page st) state) BorderLayout/CENTER)
                (.removeAll right-buttons)
                (if (:final st)
                  (let [opt-i (first (keep-indexed (fn [j s] (when (:optional s) j)) ss))]
                    (when (and opt-i (not= opt-i i)) (.add right-buttons (button (t :wiz/options) #(go! opt-i))))
                    (.add right-buttons (button (t :wiz/proceed) proceed!)))
                  (.add right-buttons (button (t :wiz/next) next!)))
                (.add right-buttons (button (t :wiz/cancel) #(.dispose dlg)))
                (.setEnabled back (pos? i))
                (doseq [^JComponent c [side body right-buttons]]
                  (.revalidate c) (.repaint c)))))
    (let [head (JPanel. (BorderLayout. 8 0))
          help (JButton. look/help-icon)
          center (JPanel. (BorderLayout.))]
      (doto back
        (.setBorderPainted false) (.setContentAreaFilled false) (.setFocusPainted false)
        (.addActionListener (reify ActionListener (actionPerformed [_ _] (go! (dec @cur))))))
      (doto help
        (.setBorderPainted false) (.setContentAreaFilled false) (.setFocusPainted false)
        (.addActionListener
         (reify ActionListener
           (actionPerformed [_ _]
             (let [st (nth (all-steps) @cur)]
               (JOptionPane/showMessageDialog
                dlg (or (:help st) (t :wiz/help-default))
                (t :wiz/help (:label st)) JOptionPane/INFORMATION_MESSAGE))))))
      (doto head
        (.setBackground (look/rgb 0xdde8f5))
        (.setBorder (BorderFactory/createEmptyBorder 4 6 4 6))
        (.add back BorderLayout/WEST)
        (.add (doto (JLabel. ^String title)
                (.setFont (look/font 18 Font/ITALIC))
                (.setForeground (look/rgb 0x1a1a1a)))
              BorderLayout/CENTER))
      (.setPreferredSize side (Dimension. 250 100))
      (.setBackground body Color/WHITE)
      (doto footer
        (.setBackground (look/footer))
        (.setBorder (BorderFactory/createMatteBorder 1 0 0 0 (look/line)))
        (.add (doto (JPanel. (FlowLayout. FlowLayout/LEFT 6 6))
                (.setOpaque false) (.add help))
              BorderLayout/WEST)
        (.add (doto right-buttons (.setOpaque false)) BorderLayout/EAST))
      (doto center
        (.add side BorderLayout/WEST)
        (.add body BorderLayout/CENTER))
      (doto (.getContentPane dlg)
        (.setLayout (BorderLayout.))
        (.add head BorderLayout/NORTH)
        (.add center BorderLayout/CENTER)
        (.add footer BorderLayout/SOUTH))
      (@render!)
      (doto dlg
        (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
        (.setSize 900 640)
        (.setLocationRelativeTo owner)
        (.setVisible true))
      (when (and @proceed? on-proceed) (on-proceed @state)))))

(defn confirm?
  "An OK/Cancel question."
  [^Component parent ^String title ^String text]
  (= JOptionPane/OK_OPTION
     (JOptionPane/showConfirmDialog parent text title JOptionPane/OK_CANCEL_OPTION
                                    JOptionPane/WARNING_MESSAGE)))
