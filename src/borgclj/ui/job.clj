(ns borgclj.ui.job
  "The one running job, in a form several threads may look at."
  (:require [borgclj.log :as log]
            [clojure.string :as str])
  (:import [java.io StringWriter]
           [java.nio.file Files OpenOption Path StandardOpenOption]))

(def ^:const log-limit 3000)

(defn new-job [] (atom {:state :idle :log [] :log-base 0 :seq 0}))

(defn- add-lines [j lines]
  (if (empty? lines)
    j
    (let [kept (into (:log j) lines)
          drop-n (max 0 (- (count kept) log-limit))]
      (-> j
          (assoc :log (subvec kept drop-n))
          (update :log-base + drop-n)
          (update :seq inc)))))

(defn- split-lines [s] (remove str/blank? (str/split s #"\r\n|\r|\n")))

(defn- take-lines!
  "Whatever the verb has printed since the last look. Only whole lines -- a
   half-written one belongs to the next round."
  [^StringBuffer buf]
  (locking buf
    (let [s (.toString buf)
          cut (inc (max (.lastIndexOf s "\n") (.lastIndexOf s "\r")))]
      (when (pos? cut)
        (.delete buf 0 cut)
        (split-lines (subs s 0 cut))))))

(defn- take-rest! [^StringBuffer buf]
  (locking buf
    (let [s (.toString buf)]
      (.setLength buf 0)
      (split-lines s))))

(defn snapshot
  "The job as it stands, after collecting what the verb printed meanwhile."
  [job]
  (locking job
    (when-let [^StringWriter w (:sink @job)]
      (swap! job add-lines (take-lines! (.getBuffer w))))
    @job))

(defn log-from
  "Log lines from a global index on, and the index to ask with next time."
  [j ^long from]
  (let [lines (:log j)
        base (long (:log-base j))]
    [(subvec lines (min (max 0 (- from base)) (count lines)))
     (+ base (count lines))]))

(defn running? [job] (= :running (:state @job)))

(defn stoppable? [j] (and (= :running (:state j)) (some? (:stop j))))

(defn stop! [job]
  (let [{:keys [^Path stop] :as j} @job]
    (when (stoppable? j)
      (Files/write stop (byte-array 0)
                   (into-array OpenOption [StandardOpenOption/CREATE
                                           StandardOpenOption/WRITE]))
      (swap! job add-lines ["Abbruch angefordert …"]))))

(defn- ended [j state line rest-lines]
  (-> j (add-lines rest-lines) (add-lines [line]) (assoc :state state :sink nil)))

(defn start!
  "Runs argv through the command line's own entry point, in a thread of its own,
   and keeps what it reports. `stop` is a path that does not exist yet: the verb
   ends cleanly once it appears, and only `image` watches for it."
  [job {:keys [run kind title argv ^Path stop]}]
  (locking job
    (when (running? job) (throw (ex-info "Es läuft bereits ein Auftrag." {})))
    (let [w (StringWriter.)]
      (reset! job {:state :running :kind kind :title title :sink w
                   :stop stop :argv argv :log [] :log-base 0 :seq 0
                   :started (System/nanoTime)})
      (swap! job add-lines [(str "$ borgi " (str/join " " argv))])
      (doto (Thread.
             ^Runnable
             (fn []
               (let [[state line]
                     (binding [*out* w
                               *err* w
                               log/*progress*
                               (fn [m] (swap! job #(-> % (assoc :progress m) (update :seq inc))))]
                       (try
                         (run argv)
                         [:done "Fertig."]
                         (catch Throwable t
                           (if (:stopped (ex-data t))
                             [:cancelled "Abgebrochen."]
                             [:failed (str "Fehlgeschlagen — "
                                           (or (ex-message t) (str (class t))))]))))]
                 (swap! job ended state line (take-rest! (.getBuffer w)))
                 (when stop
                   (Files/deleteIfExists stop)
                   (Files/deleteIfExists (.getParent stop)))))
             "borgi-ui-job")
        (.setDaemon true)
        (.start)))))
