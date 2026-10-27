(ns borgclj.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [borgclj.cli :as cli])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(def ^:private write-status! #'cli/write-status!)
(def ^:private cleanup-on-exit! #'cli/cleanup-on-exit!)
(def ^:private progress-reporter #'cli/progress-reporter)

(defn- tmp-dir ^Path [] (Files/createTempDirectory "borgclj-cli-test" (make-array FileAttribute 0)))

(deftest status-file-is-replaced-whole
  (testing "each write leaves one readable line and no leftover temp file"
    (let [dir (tmp-dir)
          p (.resolve dir "status.edn")]
      (write-status! p {:percent 1.0 :read 10})
      (write-status! p {:percent 2.5 :read 20})
      (let [m (edn/read-string (slurp (.toFile p)))]
        (is (= 2.5 (:percent m)) "the newer write wins")
        (is (= 20 (:read m))))
      (is (= ["status.edn"] (mapv #(str (.getFileName ^Path %))
                                  (iterator-seq (.iterator (Files/list dir)))))
          "the temp file must not survive the move"))))

(def ^:private eta-seconds #'cli/eta-seconds)
(def ^:private rate-window #'cli/rate-window)
(def ^:private human-time #'cli/human-time)
(def ^:private damped #'cli/damped)
(def ^:private human-span #'cli/human-span)

(deftest progress-is-measured-in-bytes-not-blocks
  (testing "the percentage follows the bytes covered, because blocks are unequal"
    (let [dir (tmp-dir)
          p (.resolve dir "run.edn")
          report (progress-reporter p true)]
      ;; block 10 of 100 would be 10%; the bytes say 25% — the cut-up grid at the
      (report {:source "C" :block 9 :of 100 :length 1000 :allocated 400
               :stats {:size 250 :read 200 :zeroed 50 :sent 120
                       :copy-nanos 1000000000 :zero-nanos 500000000}})
      (let [m (edn/read-string (slurp (.toFile p)))]
        (is (= 25.0 (:percent m)) "250 of 1000 bytes, not 10 of 100 blocks")
        (is (= 250 (:covered m)))
        (is (= 1000 (:length m)))
        (is (= 200 (:read m)))
        (is (= 200 (:to-read m)) "400 allocated, 200 read")
        (is (= 550 (:to-fill m)) "600 free, 50 already zeroed")
        (is (= 200 (:read-speed m)) "200 bytes in 1.0 s of reading")
        (is (= 100 (:fill-speed m)) "50 bytes in 0.5 s of filling")
        (is (= 6 (:eta-seconds m)) "200/200 to read plus 550/100 to fill")))))

(deftest each-source-gets-its-own-bar
  (testing "the counters run across all sources, the percentage must not"
    (let [dir (tmp-dir)
          p (.resolve dir "run.edn")
          report (progress-reporter p true)]
      (report {:source "C" :block 9 :of 10 :length 1000 :allocated 1000
               :stats {:size 1000 :read 1000 :zeroed 0 :sent 0
                       :copy-nanos 1000000000 :zero-nanos 0}})
      (is (= 100.0 (:percent (edn/read-string (slurp (.toFile p))))))
      ;; second volume: the shared counters are at 1500, but only 500 of them
      (report {:source "D" :block 9 :of 10 :length 1000 :allocated 1000
               :stats {:size 1500 :read 1500 :zeroed 0 :sent 0
                       :copy-nanos 1500000000 :zero-nanos 0}})
      (let [m (edn/read-string (slurp (.toFile p)))]
        (is (= "D" (:source m)))
        (is (= 50.0 (:percent m)) "not 150%")
        (is (= 500 (:read m)))))))

(deftest eta-splits-the-two-regimes
  (testing "reading and zero-filling are estimated apart, then added"
    (is (= 10.0 (eta-seconds 1000 0 100.0 nil)) "nothing left to fill needs no fill rate")
    (is (= 4.0 (eta-seconds 0 2000 nil 500.0)))
    (is (= 14.0 (eta-seconds 1000 2000 100.0 500.0)))
    (is (nil? (eta-seconds 1000 2000 100.0 nil))
        "work left in a regime that has not been timed yet cannot be estimated")
    (is (nil? (eta-seconds 1000 0 nil nil)))))

(deftest rate-is-taken-over-a-window
  (testing "the wall-clock rate reflects the recent seconds, not the whole run"
    (let [w (rate-window 30)
          s (long 1000000000)]
      (is (nil? (w s 0 0 0 0)) "one sample is not a rate")
      ;; one second later: 10 bytes read, 2 zeroed
      (let [m (w (+ s 1000000000) 10 2 1000000000 0)]
        (is (= 10.0 (:wall-read m)))
        (is (= 2.0 (:wall-fill m))))))
  (testing "samples older than the window are dropped, so a slump shows up"
    (let [w (rate-window 2)
          s (long 1000000000)]
      (w s 0 0 0 0)
      (w (+ s 1000000000) 1000 0 1000000000 0)
      ;; two more seconds pass with nothing read at all
      (let [m (w (+ s 3000000000) 1000 0 1000000000 0)]
        (is (< (double (:wall-read m)) 400.0)
            "the fast second has aged out; a mean since start would still say 333/s")))))

(deftest each-regime-is-timed-against-its-own-clock
  (testing "a window spent reading says nothing about how fast the filling is"
    (let [w (rate-window 30)
          s (long 1000000000)]
      (w s 0 0 0 0)
      (let [m (w (+ s 3000000000) 3000 0 3000000000 0)]
        (is (= 1000.0 (:read m)) "3000 bytes in 3 s of reading")
        (is (nil? (:fill m)) "the filling did not get slow -- it was never asked")
        (is (= 0.0 (:wall-fill m)) "against the wall clock it looks like a standstill"))))
  (testing "a window that did both times each of them apart"
    (let [w (rate-window 30)
          s (long 1000000000)]
      (w s 0 0 0 0)
      (let [m (w (+ s 4000000000) 1000 6000 1000000000 3000000000)]
        (is (= 1000.0 (:read m)) "1000 bytes in 1 s of reading")
        (is (= 2000.0 (:fill m)) "6000 bytes in 3 s of filling")
        (is (= 250.0 (:wall-read m)) "against the wall clock the reading looks four times slower")))))

(deftest a-worse-estimate-arrives-in-steps-a-better-one-at-once
  (is (= 100.0 (damped nil 100.0)) "the first value stands as it is")
  (is (= 40.0 (damped 100.0 40.0)) "falling is free")
  (is (= 120.0 (damped 100.0 200.0)) "rising covers a fifth of the distance")
  (testing "it does arrive, only not in one jump"
    (is (< 190.0 (double (reduce damped 100.0 (repeat 40 200.0))) 200.1)))
  (is (nil? (damped 100.0 nil))))

(deftest a-span-says-what-a-single-number-cannot
  (is (= "--" (human-span nil nil)))
  (is (= "1:00" (human-span 60.0 60.4)) "less than a second apart is no span")
  (is (= "1:00-2:00" (human-span 60.0 120.0)))
  (testing "printable on a Windows console, which is why the separator is a plain hyphen"
    (is (re-matches #"[\x20-\x7e]+" (human-span 60.0 120.0)))))

(deftest time-left-is-readable
  (is (= "--" (human-time nil)))
  (is (= "--" (human-time (/ 1.0 0.0))))
  (is (= "0:42" (human-time 42)))
  (is (= "5:03" (human-time 303)))
  (is (= "1:04:11" (human-time 3851))))

(deftest cleanup-runs-exactly-once
  (testing "the normal path cleans up, and does not clean up twice"
    (let [calls (atom 0)
          release! (cleanup-on-exit! #(swap! calls inc))]
      (is (= 0 @calls) "nothing runs before the command ends")
      (release!)
      (is (= 1 @calls))
      (release!)
      (is (= 1 @calls) "a second call must not release the snapshot again"))))

(deftest cleanup-swallows-its-own-failure
  (testing "a cleanup step that throws must not mask the error that caused the exit"
    (let [release! (cleanup-on-exit! #(throw (ex-info "vss said no" {})))]
      (is (nil? (try (release!) nil (catch Throwable t t)))))))

(deftest a-block-that-borg-cannot-store-is-refused-before-the-snapshot
  (testing "everything up to MAX_DATA_SIZE minus the three envelope bytes passes"
    (is (= (* 4 1024 1024) (cli/check-block (* 4 1024 1024))))
    (is (= 20971476 (cli/check-block 20971476))))
  (testing "one byte more is refused here rather than by the server at block one"
    (let [e (try (cli/check-block 20971477) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= 20971476 (:max (ex-data e))))))
  (testing "zero would leave the block grid standing at offset nought forever"
    (is (thrown? clojure.lang.ExceptionInfo (cli/check-block 0)))
    (is (thrown? clojure.lang.ExceptionInfo (cli/check-block -1)))))

(deftest a-stop-request-is-seen-without-being-looked-for-every-block
  (let [dir (tmp-dir)
        p (.resolve dir "stop")
        stop? (cli/stop-watch p)]
    (is (false? (stop?)) "nothing asked for yet")
    (Files/write p (byte-array 0) (make-array java.nio.file.OpenOption 0))
    (testing "the file is not stat'ed again straight away"
      ;; in the free tail the blocks go past in their thousands per second
      (is (false? (stop?))))
    (Thread/sleep 250)
    (is (true? (stop?)))
    (Files/delete p)
    (testing "once seen it stays seen, even when the file goes away again"
      (is (true? (stop?))))))

(deftest without-a-stop-file-nothing-is-looked-at
  (is (false? ((cli/stop-watch nil)))))
