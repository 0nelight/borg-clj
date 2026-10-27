(ns borgclj.gui.connect
  "The dialog behind \"Browse …\": where the repository is and how this
   computer logs in there. The same settings as under \"Ziel\" in the web
   interface, saved in the same file."
  (:require [borgclj.gui.data :as data]
            [borgclj.gui.i18n :refer [t]]
            [borgclj.gui.look :as look]
            [borgclj.ui.config :as config]
            [clojure.string :as str])
  (:import (java.awt BorderLayout Component Dialog$ModalityType Dimension FlowLayout
                     GridBagConstraints GridBagLayout Insets Window)
           (java.awt.event ActionListener)
           (javax.swing BorderFactory JButton JDialog JLabel JOptionPane JPanel
                        JTextField SwingUtilities WindowConstants)))

(set! *warn-on-reflection* true)

(defn- fields []
  [[:repo (t :conn/repo) (t :conn/repo-hint)]
   [:identity (t :conn/identity) (t :conn/identity-hint)]
   [:user (t :conn/user) (t :conn/user-hint)]
   [:remote-path (t :conn/remote-path) "--remote-path"]
   [:rsh (t :conn/rsh) "--rsh"]
   [:server-cmd (t :conn/server-cmd) (t :conn/server-cmd-hint)]])

(defn- button ^JButton [^String s f]
  (doto (JButton. s)
    (.addActionListener (reify ActionListener (actionPerformed [_ _] (f))))))

(defn edit!
  "Shows the dialog; true when the settings were saved."
  [^Window owner config-atom]
  (let [dlg (JDialog. owner (t :conn/title) Dialog$ModalityType/APPLICATION_MODAL)
        p (JPanel. (GridBagLayout.))
        inputs (into {} (for [[k] (fields)] [k (JTextField. (str (get @config-atom k "")) 36)]))
        saved (atom false)
        current (fn [] (into @config-atom (for [[k ^JTextField f] inputs] [k (str/trim (.getText f))])))
        status (JLabel. " ")]
    (doseq [[i [k label hint]] (map-indexed vector (fields))]
      (.add p (JLabel. (str ^String label ":"))
            (GridBagConstraints. 0 (int (* 2 i)) 1 1 0.0 0.0 GridBagConstraints/WEST
                                 GridBagConstraints/NONE (Insets. 6 12 0 8) 0 0))
      (.add p ^Component (inputs k)
            (GridBagConstraints. 1 (int (* 2 i)) 1 1 1.0 0.0 GridBagConstraints/WEST
                                 GridBagConstraints/HORIZONTAL (Insets. 6 0 0 12) 0 0))
      (.add p (doto (JLabel. ^String hint) (.setForeground (look/grey-text)) (.setFont (look/font 11)))
            (GridBagConstraints. 1 (int (inc (* 2 i))) 1 1 1.0 0.0 GridBagConstraints/WEST
                                 GridBagConstraints/NONE (Insets. 0 0 0 12) 0 0)))
    (.add p status (GridBagConstraints. 0 (int (* 2 (count (fields)))) 2 1 1.0 0.0
                                        GridBagConstraints/WEST GridBagConstraints/HORIZONTAL
                                        (Insets. 12 12 6 12) 0 0))
    (.setBackground p java.awt.Color/WHITE)
    (let [test (button (t :conn/test)
                       (fn []
                         (.setText status (t :conn/testing))
                         (let [c (current)]
                           (doto (Thread.
                                  ^Runnable
                                  (fn []
                                    (let [msg (try (t :conn/reachable (count (data/archives c)))
                                                   (catch Throwable e
                                                     (t :conn/unreachable (ex-message e))))]
                                      (SwingUtilities/invokeLater #(.setText status msg)))))
                             (.setDaemon true) (.start)))))
          ok (button "OK" (fn []
                            (let [c (current)]
                              (reset! config-atom c)
                              (try (config/save! c)
                                   (catch Throwable e
                                     (JOptionPane/showMessageDialog
                                      dlg (t :conn/not-saved (ex-message e)))))
                              (reset! saved true)
                              (.dispose dlg))))
          cancel (button (t :wiz/cancel) #(.dispose dlg))]
      (doto (.getContentPane dlg)
        (.setLayout (BorderLayout.))
        (.add (look/heading (t :conn/heading)) BorderLayout/NORTH)
        (.add p BorderLayout/CENTER)
        (.add (doto (JPanel. (BorderLayout.))
                (.setBackground (look/footer))
                (.setBorder (BorderFactory/createMatteBorder 1 0 0 0 (look/line)))
                (.add (doto (JPanel. (FlowLayout. FlowLayout/LEFT 8 8)) (.setOpaque false) (.add test))
                      BorderLayout/WEST)
                (.add (doto (JPanel. (FlowLayout. FlowLayout/RIGHT 8 8)) (.setOpaque false)
                        (.add ok) (.add cancel))
                      BorderLayout/EAST))
              BorderLayout/SOUTH))
      (.setBackground (.getContentPane dlg) java.awt.Color/WHITE)
      (doto dlg
        (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
        (.setSize (Dimension. 640 470))
        (.setLocationRelativeTo owner)
        (.setVisible true)))
    @saved))
