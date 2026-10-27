(ns borgclj.main
  (:gen-class)
  (:require [borgclj.cli :as cli]
            [clojure.string :as str]))

(defn- debug? [argv]
  (or (some #{"--debug"} argv)
      (when-let [v (System/getenv "BORG_DEBUG")]
        (not (#{"" "0" "false" "no"} (str/lower-case v))))))

(defn- print-cause-chain [^Throwable e]
  (loop [^Throwable c e]
    (when c
      (println (format "  %s: %s" (.getName (class c)) (or (.getMessage c) "")))
      (when-let [d (ex-data c)]
        (println (str "    ex-data: " (pr-str d))))
      (recur (.getCause c)))))

(defn -main [& argv]
  (let [dbg (debug? argv)]
    (try
      (cli/run (vec (remove #{"--debug"} argv)))
      (flush)
      (System/exit 0)
      (catch Throwable e
        ;; a run that was asked to stop did what it was told; that is not a
        (when (:stopped (ex-data e))
          (binding [*out* *err*]
            (println "borgi: stopped on request -- nothing was committed")
            (flush))
          (System/exit 3))
        (binding [*out* *err*]
          (println "borgi:" (or (ex-message e) (str (class e))))
          (when-let [d (ex-data e)]
            (when-not dbg (println "  detail:" (pr-str d))))
          (when dbg
            (println "--- cause chain ---")
            (print-cause-chain e)
            (println "--- stack trace ---")
            (.printStackTrace e))
          (flush))
        (System/exit 2)))))
