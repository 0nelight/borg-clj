(ns borgclj.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [borgclj.cli :as cli]
            [borgclj.log :as log]
            [borgclj.ui.config :as config]
            [borgclj.ui.html :as h]
            [borgclj.ui.job :as job]
            [borgclj.ui.pages :as pages]
            [borgclj.ui.server :as server])
  (:import [java.net HttpURLConnection URI]
           [java.nio.file Files LinkOption Path]))

(defn- fetch
  "Code and body of one GET, without following the redirect."
  [^String url]
  (let [^HttpURLConnection c (.openConnection (.toURL (URI/create url)))]
    (.setInstanceFollowRedirects c false)
    (let [code (.getResponseCode c)
          body (slurp (or (.getErrorStream c) (.getInputStream c)))]
      [code body])))

(defn- with-server [run f]
  (let [{:keys [server app url]} (binding [*out* (java.io.StringWriter.)]
                                   (server/serve! {:port 0 :no-browser true :run run}))
        base (subs url 0 (str/index-of url "/?"))
        token (subs url (+ 4 (str/index-of url "/?t=")))]
    (try (f {:app app :url url :base base :token token}) (finally (.stop server 0)))))

(defn- wait-until [pred]
  (loop [n 0]
    (cond (pred) true
          (> n 400) false
          :else (do (Thread/sleep 25) (recur (inc n))))))

(deftest a-request-without-the-one-time-token-is-refused
  (with-server (fn [_] nil)
    (fn [{:keys [url base token]}]
      (is (= 403 (first (fetch (str base "/uebersicht")))))
      (is (= 403 (first (fetch (str base "/uebersicht?t=nichtdertoken")))))
      (testing "the link the program printed gets in"
        (is (= 302 (first (fetch url)))))
      (testing "and the token works on any page, not only the entry point"
        (is (= 200 (first (fetch (str base "/lauf?t=" token)))))))))

(deftest the-run-page-refreshes-itself-where-there-is-no-javascript
  (let [gate (promise)]
    (with-server (fn [_] nil)
      (fn [{:keys [app base token]}]
        (job/start! (:job app) {:kind :image :title "Sicherung t1" :argv ["image" "r::t1" "C:"]
                                :run (fn [_] (println "läuft") @gate)})
        (let [[code body] (fetch (str base "/lauf?t=" token))]
          (is (= 200 code))
          (is (str/includes? body "<noscript><meta http-equiv=\"refresh\" content=\"2\"></noscript>")
              "this is what carries the recovery system, where w3m is the whole browser")
          (is (str/includes? body "<script src=\"/app.js\" defer></script>")
              "and this is what spares a capable browser the reload"))
        (deliver gate nil)
        (is (wait-until #(= :done (:state (job/snapshot (:job app))))))
        (let [[_ body] (fetch (str base "/lauf?t=" token))]
          (is (not (str/includes? body "<script"))
              "a finished page has nothing left to update")
          (is (str/includes? body "Fertig."))
          (is (str/includes? body "läuft") "and still carries the protocol"))
        (testing "the stylesheet is served out of the binary itself"
          (let [[code body] (fetch (str base "/app.css?t=" token))]
            (is (= 200 code))
            (is (str/includes? body ".bar"))))))))

(deftest the-argv-is-the-one-a-person-could-have-typed
  (let [cfg (assoc config/defaults :repo "ssh://b@h/srv/repo" :compression "zstd,9"
                   :block "8M" :snapshot "vshadow" :identity "/k" :init true)
        argv (config/image-args cfg {:archive "a1" :sources ["C:" "disk:0"] :dry-run true})]
    (is (= ["image" "ssh://b@h/srv/repo::a1" "C:" "disk:0"] (subvec argv 0 4)))
    (is (= ["--identity" "/k"] (subvec argv (- (count argv) 2))))
    (doseq [[o v] [["--compression" "zstd,9"] ["--block" "8M"] ["--snapshot" "vshadow"]]]
      (is (= v (nth argv (inc (.indexOf ^java.util.List argv o)))) o))
    (is (some #{"--init"} argv))
    (is (some #{"--dry-run"} argv))
    (testing "the interface reads progress from the run, not from its output"
      (is (some #{"--quiet"} argv))))
  (testing "auto is the default, so it is not worth an argument"
    (is (not (some #{"--snapshot"} (config/image-args (assoc config/defaults :repo "r")
                                                      {:archive "a" :sources ["C:"]})))))
  (testing "a request that cannot become a command says so before anything starts"
    (is (thrown? clojure.lang.ExceptionInfo
                 (config/image-args (assoc config/defaults :repo "r")
                                    {:archive "a" :sources []})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (config/restore-args (assoc config/defaults :repo "r")
                                      {:archive "a1" :target ""})))))

(deftest the-repository-is-named-once-and-checked-once
  (let [cfg (assoc config/defaults :repo " ssh://h/srv/repo ")]
    (is (= "ssh://h/srv/repo" (config/location cfg nil)))
    (is (= "ssh://h/srv/repo::a1" (config/location cfg "a1"))))
  (is (thrown? clojure.lang.ExceptionInfo (config/location config/defaults "a1"))))

(deftest the-archive-name-is-filled-in-before-it-is-shown
  (let [nm (config/archive-name "{hostname}-{date}")]
    (is (re-matches #"[^ ]+-\d{4}-\d{2}-\d{2}" nm))
    (is (not (str/includes? nm "{")))))

(defn- wait-for [job state]
  (wait-until #(= state (:state (job/snapshot job)))))

(deftest a-job-keeps-what-the-verb-printed-and-what-it-reported
  (let [j (job/new-job)]
    (job/start! j {:kind :image :title "Probelauf" :argv ["image" "repo::a" "C:"]
                   :run (fn [_]
                          (println "erste Zeile")
                          (log/*progress* {:phase :copy :source "C:" :percent 40.0
                                           :read 1024 :zeroed 0 :sent 512
                                           :read-per-second 256 :eta-seconds 90})
                          (binding [*out* *err*] (println "zweite Zeile")))})
    (is (wait-for j :done))
    (let [s (job/snapshot j)]
      (is (= :done (:state s)))
      (is (= ["$ borgi image repo::a C:" "erste Zeile" "zweite Zeile" "Fertig."] (:log s))
          "stdout and stderr land in the one protocol, in order")
      (is (= 100.0 (pages/percent-of s)) "a finished run is finished")
      (is (str/includes? (pages/numbers-of s) "1.00 kB gelesen")))))

(deftest a-job-that-throws-is-reported-not-swallowed
  (let [j (job/new-job)]
    (job/start! j {:kind :image :title "x" :argv ["image"]
                   :run (fn [_] (throw (ex-info "kein Repository" {})))})
    (is (wait-for j :failed))
    (is (str/includes? (last (:log (job/snapshot j))) "kein Repository"))))

(deftest only-a-job-that-watches-for-it-offers-to-be-stopped
  (let [dir (Files/createTempDirectory "borgclj-ui-test"
                                       (make-array java.nio.file.attribute.FileAttribute 0))
        stop (.resolve dir "stop")
        j (job/new-job)]
    (job/start! j {:kind :image :title "x" :argv ["image"] :stop stop
                   :run (fn [_]
                          (let [stop? (cli/stop-watch stop)]
                            (loop []
                              (if (stop?)
                                (throw (ex-info "stopped" {:stopped true :block 3 :of 9}))
                                (do (Thread/sleep 25) (recur))))))})
    (is (job/stoppable? (job/snapshot j)))
    (job/stop! j)
    (is (wait-for j :cancelled))
    (is (some #{"Abbruch angefordert …"} (:log (job/snapshot j))))
    (testing "the request is cleared away again, so the next run is not stopped at birth"
      (is (wait-until #(not (Files/exists stop (make-array LinkOption 0))))))
    (Files/deleteIfExists ^Path dir))
  (testing "a job with nothing watching does not grow a button that would do nothing"
    (let [j (job/new-job)
          gate (promise)]
      (job/start! j {:kind :fetch :title "x" :argv ["get"] :run (fn [_] @gate)})
      (is (not (job/stoppable? (job/snapshot j))))
      (deliver gate nil))))

(deftest the-protocol-is-a-ring-and-a-reader-can-follow-it
  (let [j (job/new-job)
        gate (promise)]
    (job/start! j {:kind :image :title "x" :argv ["image"]
                   :run (fn [_] (dotimes [i (+ job/log-limit 100)] (println (str "z" i)))
                          @gate)})
    (Thread/sleep 200)
    (let [s (job/snapshot j)]
      (is (= job/log-limit (count (:log s))) "the oldest lines fall off the front")
      (is (pos? (:log-base s)))
      (testing "a reader that fell behind the ring is carried to where it starts"
        (let [[lines idx] (job/log-from s 0)]
          (is (= job/log-limit (count lines)))
          (is (= (+ (:log-base s) job/log-limit) idx))
          (is (empty? (first (job/log-from s idx))) "and asking again brings nothing new"))))
    (deliver gate nil)))

(deftest the-event-payload-says-what-the-page-says
  (let [s {:state :running :seq 3
           :progress {:phase :copy :source "C:" :percent 12.5 :read 2048 :zeroed 1024
                      :sent 512 :read-per-second 1024 :eta-seconds 61}}
        p (pages/event-payload s ["eine \"Zeile\"" "noch eine"])]
    (is (str/includes? p "\"percent\":12.5"))
    (is (str/includes? p "\"state\":\"running\""))
    (is (str/includes? p "noch etwa 01:01"))
    (testing "a quote in a log line must not end the JSON string"
      (is (str/includes? p "eine \\\"Zeile\\\"")))))

(deftest markup-cannot-come-out-of-a-name
  (is (= "&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt; &amp;"
         (h/esc "<script>alert(\"x\")</script> &"))))

(deftest a-page-is-readable-with-no-javascript-at-all
  (let [body (h/layout {:active "/lauf" :title "Auftrag" :subtitle "Der Lauf." :head ""
                        :body "<p>40,0 %</p>"})]
    (is (not (str/includes? body "<script")))
    (testing "every page is reachable as a plain link, not through a handler"
      (doseq [href ["/uebersicht" "/ziel" "/sichern" "/sicherungen" "/dateien" "/zurueck"]]
        (is (str/includes? body (str "<a href=\"" href "\"")))))))
