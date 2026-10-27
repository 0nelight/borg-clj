(ns borgclj.gui.look
  "Colours, gradients, icons and the small parts the windows are built from.
   The model is the Acronis True Image rescue media: a blue bar on the left with
   the steps, a white page on the right, an italic blue heading, a dark blue
   toolbar above tables, a grey button bar.

   Everything is drawn, nothing is loaded from image files: the program stays a
   single file, and it looks the same on Windows and under X."
  (:import (java.awt BasicStroke Color Component Cursor Dimension Font
                     GradientPaint Graphics Graphics2D RenderingHints)
           (java.awt.event MouseAdapter MouseEvent)
           (java.awt.geom Ellipse2D$Double GeneralPath RoundRectangle2D$Double)
           (javax.swing BorderFactory Icon JComponent JLabel JPanel SwingConstants)))

(set! *warn-on-reflection* true)

(defn rgb ^Color [^long v] (Color. (int v)))

;; Colours are functions, not values: borgclj is initialised when the native
;; image is built, and a java.awt.Color created then would have to live in the
;; image heap -- which native-image refuses for AWT classes. The delay makes
;; each one once, at run time.
(defmacro ^:private defcolor [nm v]
  `(let [d# (delay (Color. (int ~v)))]
     (defn ~nm ~(with-meta [] {:tag 'java.awt.Color}) @d#)))

(defcolor side-top 0x2f5f98)
(defcolor side-bottom 0x7ea7d8)
(defcolor title-blue 0x1d4f91)
(defcolor bar-top 0x5078a8)
(defcolor bar-bottom 0x23466f)
(defcolor footer 0xeef1f5)
(defcolor line 0xb9c3cf)
(defcolor green 0x3aa635)
(defcolor grey-text 0x9aa9bd)
(defcolor link-blue 0x1f5fb5)
(defcolor select-blue 0x3d8ee8)

(defn font
  (^Font [^long size] (font size Font/PLAIN))
  (^Font [^long size ^long style] (Font. Font/SANS_SERIF (int style) (int size))))

(defn- smooth ^Graphics2D [^Graphics g]
  (let [g2 ^Graphics2D (.create g)]
    (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g2 RenderingHints/KEY_TEXT_ANTIALIASING
                       RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    g2))

(defn gradient-panel
  "A panel with a vertical gradient and the light swoosh of the Acronis
   side bar."
  (^JPanel [^Color top ^Color bottom] (gradient-panel top bottom false))
  (^JPanel [^Color top ^Color bottom swoosh?]
   (doto (proxy [JPanel] []
           (paintComponent [^Graphics g]
             (let [^JPanel this this
                   g2 (smooth g)
                   w (.getWidth this) h (.getHeight this)]
               (.setPaint g2 (GradientPaint. 0 0 top 0 h bottom))
               (.fillRect g2 0 0 w h)
               (when swoosh?
                 (.setPaint g2 (Color. 255 255 255 38))
                 (let [p (GeneralPath.)]
                   (.moveTo p 0.0 (* h 0.55))
                   (.curveTo p (* w 0.5) (* h 0.48) (* w 0.8) (* h 0.62) (double w) (* h 0.52))
                   (.lineTo p (double w) (double h))
                   (.lineTo p 0.0 (double h))
                   (.closePath p)
                   (.fill g2 p)))
               (.dispose g2))))
     (.setOpaque true))))

(defn bar
  "The dark blue toolbar above a table."
  ^JPanel []
  (doto (gradient-panel (bar-top) (bar-bottom))
    (.setLayout (java.awt.BorderLayout.))
    (.setBorder (BorderFactory/createEmptyBorder 4 8 4 8))
    (.setPreferredSize (Dimension. 10 28))))

(defn white-label ^JLabel [^String s]
  (doto (JLabel. s)
    (.setForeground Color/WHITE)
    (.setFont (font 13 Font/ITALIC))))

(defn heading
  "The italic blue heading of a wizard step."
  ^JLabel [^String s]
  (doto (JLabel. s)
    (.setForeground (title-blue))
    (.setFont (font 18 Font/ITALIC))
    (.setBorder (BorderFactory/createEmptyBorder 10 10 8 10))))

(defn link
  "An underlined link that calls f."
  ^JLabel [^String s f]
  (let [l (JLabel. (str "<html><u>" s "</u></html>"))]
    (doto l
      (.setForeground (link-blue))
      (.setFont (font 14))
      (.setCursor (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
      (.addMouseListener (proxy [MouseAdapter] []
                           (mouseClicked [^MouseEvent _] (f)))))))

(defn clickable! ^Component [^Component c f]
  (.setCursor c (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
  (.addMouseListener c (proxy [MouseAdapter] []
                         (mouseClicked [^MouseEvent _] (f))))
  c)

;; --- icons --------------------------------------------------------------------

(defn- icon ^Icon [^long w ^long h paint]
  (reify Icon
    (getIconWidth [_] (int w))
    (getIconHeight [_] (int h))
    (paintIcon [_ _ g x y]
      (let [g2 (smooth g)]
        (.translate g2 (int x) (int y))
        (paint g2)
        (.dispose g2)))))

(def ^Icon check-icon
  (icon 16 16 (fn [^Graphics2D g]
                (.setColor g (green))
                (.setStroke g (BasicStroke. 2.6 BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
                (let [p (GeneralPath.)]
                  (.moveTo p 3.0 8.0) (.lineTo p 6.5 12.5) (.lineTo p 13.0 3.0)
                  (.draw g p)))))

(def ^Icon arrow-icon
  (icon 16 16 (fn [^Graphics2D g]
                (.setPaint g (GradientPaint. 0 0 (rgb 0x7fd06a) 0 16 (rgb 0x2f9a2a)))
                (let [p (GeneralPath.)]
                  (.moveTo p 1.0 5.5) (.lineTo p 8.0 5.5) (.lineTo p 8.0 1.5)
                  (.lineTo p 15.0 8.0) (.lineTo p 8.0 14.5) (.lineTo p 8.0 10.5)
                  (.lineTo p 1.0 10.5) (.closePath p)
                  (.fill g p)))))

(def ^Icon blank-icon (icon 16 16 (fn [_])))

(defn disk-icon
  "A small hard disk; a large one for the home page."
  (^Icon [] (disk-icon 16))
  (^Icon [^long s]
   (icon s s (fn [^Graphics2D g]
               (let [w (* 0.9 s) h (* 0.5 s) y (* 0.35 s)]
                 (.setPaint g (GradientPaint. 0 (float y) (rgb 0xf4f6f8) 0 (float (+ y h)) (rgb 0xa9b3bd)))
                 (.fill g (RoundRectangle2D$Double. (* 0.05 s) y w h (* 0.15 s) (* 0.15 s)))
                 (.setColor g (rgb 0x6b7783))
                 (.draw g (RoundRectangle2D$Double. (* 0.05 s) y w h (* 0.15 s) (* 0.15 s)))
                 (.setColor g (rgb 0x3aa635))
                 (.fill g (Ellipse2D$Double. (* 0.75 s) (+ y (* 0.3 h)) (* 0.08 s) (* 0.08 s))))))))

(defn big-disk-icon
  "The disk with an arrow on the home page: up for backup, down for
   recovery."
  ^Icon [up?]
  (icon 48 44 (fn [^Graphics2D g]
                (let [^Icon d (disk-icon 44)]
                  (.paintIcon d nil g 0 6))
                (let [p (GeneralPath.)
                      [c1 c2] (if up? [(rgb 0x9fd6ff) (rgb 0x2f7fd0)] [(rgb 0x8fe07a) (rgb 0x2f9a2a)])]
                  (.setPaint g (GradientPaint. 0 0 c1 0 24 c2))
                  (if up?
                    (do (.moveTo p 22.0 0.0) (.lineTo p 32.0 10.0) (.lineTo p 26.0 10.0)
                        (.lineTo p 26.0 20.0) (.lineTo p 18.0 20.0) (.lineTo p 18.0 10.0)
                        (.lineTo p 12.0 10.0))
                    (do (.moveTo p 18.0 0.0) (.lineTo p 26.0 0.0) (.lineTo p 26.0 10.0)
                        (.lineTo p 32.0 10.0) (.lineTo p 22.0 20.0) (.lineTo p 12.0 10.0)
                        (.lineTo p 18.0 10.0)))
                  (.closePath p)
                  (.fill g p)
                  (.setColor g (Color. 0 0 0 60))
                  (.draw g p)))))

(def ^Icon back-icon
  (icon 30 30 (fn [^Graphics2D g]
                (.setPaint g (GradientPaint. 0 0 (rgb 0x6fa8e8) 0 30 (rgb 0x1d4f91)))
                (.fill g (Ellipse2D$Double. 1.0 1.0 28.0 28.0))
                (.setColor g Color/WHITE)
                (.setStroke g (BasicStroke. 1.5))
                (.draw g (Ellipse2D$Double. 1.0 1.0 28.0 28.0))
                (let [p (GeneralPath.)]
                  (.moveTo p 7.0 15.0) (.lineTo p 15.0 7.5) (.lineTo p 15.0 12.0)
                  (.lineTo p 23.0 12.0) (.lineTo p 23.0 18.0) (.lineTo p 15.0 18.0)
                  (.lineTo p 15.0 22.5) (.closePath p)
                  (.fill g p)))))

(def ^Icon help-icon
  (icon 24 24 (fn [^Graphics2D g]
                (.setPaint g (GradientPaint. 0 0 (rgb 0x6fa8e8) 0 24 (rgb 0x1d5fb0)))
                (.fill g (Ellipse2D$Double. 1.0 1.0 22.0 22.0))
                (.setColor g Color/WHITE)
                (.setFont g (font 15 Font/BOLD))
                (.drawString g "?" (int 8) (int 18)))))

(def ^Icon info-icon
  (icon 28 28 (fn [^Graphics2D g]
                (.setPaint g (GradientPaint. 0 0 (rgb 0x9fcaf5) 0 28 (rgb 0x2f78c8)))
                (.fill g (Ellipse2D$Double. 1.0 1.0 26.0 26.0))
                (.setColor g Color/WHITE)
                (.setFont g (font 17 Font/BOLD))
                (.drawString g "i" (int 12) (int 21)))))

(def ^Icon folder-icon
  (icon 16 16 (fn [^Graphics2D g]
                (.setColor g (rgb 0xe8c25a))
                (.fillRect g 1 4 14 10)
                (.fillRect g 1 2 6 3)
                (.setColor g (rgb 0xb58a22))
                (.drawRect g 1 4 14 10))))

(def ^Icon file-icon
  (icon 16 16 (fn [^Graphics2D g]
                (.setColor g Color/WHITE)
                (.fillRect g 3 1 10 14)
                (.setColor g (rgb 0x7b8794))
                (.drawRect g 3 1 10 14)
                (.drawLine g 5 5 11 5) (.drawLine g 5 8 11 8) (.drawLine g 5 11 9 11))))

(defn centered ^JLabel [^String s]
  (doto (JLabel. s SwingConstants/CENTER) (.setForeground (grey-text))))

(defn pad! ^JComponent [^JComponent c t l b r]
  (.setBorder c (BorderFactory/createEmptyBorder (int t) (int l) (int b) (int r)))
  c)
