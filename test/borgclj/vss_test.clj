(ns borgclj.vss-test
  "A snapshot outlives the run that took it, and on Windows a run is ended with
   TerminateProcess -- so nothing inside the process gets to tidy up."
  (:require [clojure.test :refer [deftest is testing]]
            [borgclj.win.vss :as vss])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-registry ^Path []
  (.resolve (Files/createTempDirectory "borgclj-vss" (make-array FileAttribute 0))
            "snapshots.edn"))

(defn- pid-that-is-not-running ^long []
  (inc (reduce max 1 (map #(.pid ^java.lang.ProcessHandle %)
                          (iterator-seq (.iterator (java.lang.ProcessHandle/allProcesses)))))))

(deftest what-was-taken-is-written-down-and-can-be-forgotten
  (binding [vss/*registry* (tmp-registry)]
    (testing "nothing noted yet reads as nothing, not as a failure"
      (is (= [] (vss/recorded))))
    (vss/remember! {:provider :wmi :ids ["{aaa}"]} ["C"])
    (vss/remember! {:provider :wmi :ids ["{bbb}"]} ["D"])
    (let [rs (vss/recorded)]
      (is (= 2 (count rs)))
      (is (= #{["{aaa}"] ["{bbb}"]} (set (map :ids rs))))
      (is (every? #(= (.pid (java.lang.ProcessHandle/current)) (:pid %)) rs)))
    (vss/forget! ["{aaa}"])
    (is (= [["{bbb}"]] (map :ids (vss/recorded))))))

(deftest an-unreadable-note-is-worth-no-more-than-no-note
  (binding [vss/*registry* (tmp-registry)]
    (Files/createDirectories (.getParent ^Path vss/*registry*) (make-array FileAttribute 0))
    (Files/write ^Path vss/*registry* (.getBytes "{ dies ist kein edn" "UTF-8")
                 (make-array java.nio.file.OpenOption 0))
    (is (= [] (vss/recorded)) "eine kaputte Notiz darf kein Backup scheitern lassen")))

(deftest a-note-belongs-to-the-process-that-made-it
  (let [me (java.lang.ProcessHandle/current)
        started (.toEpochMilli (.orElse (.startInstant (.info me)) nil))]
    (testing "the running owner keeps its snapshot"
      (is (vss/owner-alive? {:pid (.pid me) :started started})))
    (testing "a pid nobody is using has no owner left"
      (is (not (vss/owner-alive? {:pid (pid-that-is-not-running) :started started}))))
    (testing "the same pid with another start time is another process"
      ;; pids come round again. Without this the tidying would release the
      (is (not (vss/owner-alive? {:pid (.pid me) :started (- started 1000)}))))
    (testing "a note from a version that did not record the start time still counts"
      (is (vss/owner-alive? {:pid (.pid me) :started nil})))))

(deftest only-what-has-no-owner-is-released
  (binding [vss/*registry* (tmp-registry)]
    (let [me (java.lang.ProcessHandle/current)
          started (.toEpochMilli (.orElse (.startInstant (.info me)) nil))
          mine {:provider :wmi :ids ["{lebt}"] :pid (.pid me) :started started}
          orphan {:provider :wmi :ids ["{verwaist}"] :pid (pid-that-is-not-running) :started started}
          [dead alive] (vss/stale [mine orphan] vss/owner-alive?)]
      (is (= [["{verwaist}"]] (map :ids dead)))
      (is (= [["{lebt}"]] (map :ids alive))))))
