(ns borgclj.log
  (:require [clojure.string :as str])
  (:import [java.time OffsetDateTime]))

(defn tty?
  "True when stderr is a terminal."
  []
  (try
    (boolean (some-> ^java.io.Console (System/console) (.isTerminal)))
    (catch Throwable _ false)))

(defn stamp
  "Local time with its offset."
  ^String []
  (str (OffsetDateTime/now)))

(defn line
  "One timestamped line on stderr, flushed. Everything that is not the progress
   line goes through here, so a log can be read back against the clock."
  [& parts]
  (binding [*out* *err*]
    (println (str (stamp) "  " (str/join " " (map str parts))))
    (flush)))

(defn fmt
  "format under Locale/ROOT — a log gets parsed, and the JVM would otherwise
   write numbers with whatever decimal mark the machine happens to use."
  ^String [^String f & args]
  (String/format java.util.Locale/ROOT f (into-array Object args)))

(def ^:dynamic *progress*
  "Where a run reports its progress besides --status. The interface binds this
   to watch a verb it is running in its own process."
  nil)

(defn human
  "A byte count the way every message in the program prints one."
  ^String [^long n]
  (let [u ["B" "kB" "MB" "GB" "TB" "PB"]]
    (loop [v (double n) i 0]
      (if (or (< v 1024.0) (= i 5))
        (fmt (if (zero? i) "%.0f %s" "%.2f %s") v (nth u i))
        (recur (/ v 1024.0) (inc i))))))

(defn since ^double [^long started] (/ (double (- (System/nanoTime) started)) 1e9))

(defn step
  "Names the phase that just finished, how long it took and what it produced."
  [what ^long started detail]
  (line (String/format java.util.Locale/ROOT "%-9s %7.1fs  %s"
                       (into-array Object [(str what) (since started) (str detail)]))))
