(ns borgclj.remote-test
  (:require [clojure.test :refer [deftest is testing]]
            [borgclj.bytes :as b]
            [borgclj.mpack :as pack]
            [borgclj.msgpack :as mp]
            [borgclj.remote :as remote])
  (:import [java.io ByteArrayOutputStream PipedInputStream PipedOutputStream]))

(defn- read-request
  "One msgpack request off the wire, or nil at end of stream."
  [^java.io.InputStream in ^ByteArrayOutputStream carry]
  (loop []
    (let [^bytes data (.toByteArray carry)
          parsed (when (pos? (alength data)) (try (mp/read-val data 0) (catch Exception _ nil)))]
      (if parsed
        (let [[v off] parsed
              rest- (java.util.Arrays/copyOfRange data (int off) (alength data))]
          (.reset carry)
          (.write carry rest- 0 (alength rest-))
          v)
        (let [c (.read in)]
          (when-not (neg? c)
            (.write carry c)
            (recur)))))))

(defn- fake-serve
  "A borg serve that answers only once it has seen `hold` requests, so a client
   that waits for each acknowledgement deadlocks instead of passing the
   test."
  [{:keys [hold fail-at]}]
  (let [to-server (PipedOutputStream.)
        from-server (PipedInputStream. (* 1024 1024))
        server-in (PipedInputStream. to-server (* 8 1024 1024))
        server-out (PipedOutputStream. from-server)
        seen (atom [])
        thread (future
                 (let [carry (ByteArrayOutputStream.)]
                   (loop [queued []]
                     (if-let [req (read-request server-in carry)]
                       (let [msgid (:i req)
                             method (b/latin1 (:m req))
                             queued (conj queued [msgid method])]
                         (swap! seen conj [msgid method])
                         (if (and (= method "put") (< (count queued) (long hold)))
                           (recur queued)
                           (do (doseq [[id m] queued]
                                 (.write server-out
                                         ^bytes (pack/pack
                                                  (if (and fail-at (= id (long fail-at)))
                                                    {"i" id
                                                     "exception_class" "IntegrityError"
                                                     "exception_short" ["chunk rejected"]}
                                                    {"i" id "r" (when (= m "commit") true)})))
                                 (.flush server-out))
                               (recur []))))
                       :eof))))]
    {:conn (remote/->Conn to-server from-server (atom 0) (atom (byte-array 0))
                          (java.util.HashSet.) (fn [] (.close to-server) 0))
     :seen seen
     :thread thread}))

(deftest put-does-not-wait-for-each-acknowledgement
  (testing "ten puts reach borg serve before a single answer comes back"
    (let [{:keys [conn seen]} (fake-serve {:hold 10})
          done (future
                 (dotimes [i 10]
                   (remote/put! conn (byte-array 32 (byte i)) (byte-array 1024 (byte i))))
                 (remote/drain-puts! conn)
                 :done)]
      (is (= :done (deref done 5000 :timed-out))
          "a client that waits per put cannot finish here")
      (is (= 10 (count (filter #(= "put" (second %)) @seen))))
      (is (= (range 1 11) (map first @seen))))))

(deftest commit-waits-for-every-outstanding-put
  (testing "the acknowledgements are reconciled before the commit is answered"
    (let [{:keys [conn seen]} (fake-serve {:hold 4})
          done (future
                 (dotimes [i 4]
                   (remote/put! conn (byte-array 32 (byte i)) (byte-array 64 (byte i))))
                 (remote/commit! conn)
                 :done)]
      (is (= :done (deref done 5000 :timed-out)))
      (is (= ["put" "put" "put" "put" "commit"] (map second @seen)))
      (is (.isEmpty ^java.util.Set (:pending conn))
          "nothing may stay unacknowledged once the commit returned"))))

(deftest a_failed_put_surfaces_before_the_commit
  (testing "an error on the third put is raised, not swallowed by the window"
    (let [{:keys [conn]} (fake-serve {:hold 3 :fail-at 3})
          outcome (future
                    (try
                      (dotimes [i 3]
                        (remote/put! conn (byte-array 32 (byte i)) (byte-array 64 (byte i))))
                      (remote/commit! conn)
                      :committed
                      (catch clojure.lang.ExceptionInfo e
                        [:raised (.getMessage e)])))
          r (deref outcome 5000 :timed-out)]
      (is (vector? r) (str "expected the remote error to be raised, got " r))
      (is (re-find #"IntegrityError" (second r))))))

(defn- fake-open-server
  "A borg serve that refuses to create over an existing repository, the way the
   real one does — with the exception class it happens to raise."
  [^String refusal]
  (let [to-server (PipedOutputStream.)
        from-server (PipedInputStream. (* 64 1024))
        server-in (PipedInputStream. to-server (* 64 1024))
        server-out (PipedOutputStream. from-server)
        seen (atom [])]
    (future
      (let [carry (ByteArrayOutputStream.)]
        (loop []
          (when-let [req (read-request server-in carry)]
            (let [msgid (:i req)
                  method (b/latin1 (:m req))
                  create (get-in req [:a :create])]
              (swap! seen conj [method create])
              (.write server-out
                      ^bytes (pack/pack
                               (cond
                                 (= method "negotiate") {"i" msgid "r" {}}
                                 (and (= method "open") create)
                                 {"i" msgid
                                  "exception_class" refusal
                                  "exception_short" ["refused at /srv/repo"]}
                                 :else {"i" msgid "r" (byte-array 32 (byte 7))})))
              (.flush server-out))
            (recur)))))
    {:conn (remote/->Conn to-server from-server (atom 0) (atom (byte-array 0))
                          (java.util.HashSet.) (fn [] (.close to-server) 0))
     :seen seen}))

(deftest init-opens-a-repository-that-already-exists
  (testing "--init means create if missing, not fail when present"
    ;; Which refusal comes back depends on how far a previous creation got. A
    (doseq [refusal ["AlreadyExists" "FileExistsError"]]
      (let [{:keys [conn seen]} (fake-open-server refusal)
            id (future (remote/open! conn "/srv/repo" :create true))
            r (deref id 5000 :timed-out)]
        (is (bytes? r) (str refusal ": expected the repository id, got " r))
        (is (= [["negotiate" nil] ["open" true] ["open" false]] @seen)
            (str refusal ": the refused create is retried as a plain open, on the same connection"))))))

(deftest open-still-reports-other-failures
  (testing "a refusal that stands without create too must reach the caller"
    (let [to-server (PipedOutputStream.)
          from-server (PipedInputStream. (* 64 1024))
          server-in (PipedInputStream. to-server (* 64 1024))
          server-out (PipedOutputStream. from-server)
          conn (remote/->Conn to-server from-server (atom 0) (atom (byte-array 0))
                              (java.util.HashSet.) (fn [] (.close to-server) 0))]
      (future
        (let [carry (ByteArrayOutputStream.)]
          (loop []
            (when-let [req (read-request server-in carry)]
              (let [msgid (:i req)
                    method (b/latin1 (:m req))]
                (.write server-out
                        ^bytes (pack/pack (if (= method "negotiate")
                                            {"i" msgid "r" {}}
                                            {"i" msgid
                                             "exception_class" "PathNotAllowed"
                                             "exception_short" ["nope"]})))
                (.flush server-out))
              (recur)))))
      (let [outcome (future (try (remote/open! conn "/srv/repo" :create true)
                                 (catch clojure.lang.ExceptionInfo e [:raised (.getMessage e)])))
            r (deref outcome 5000 :timed-out)]
        (is (vector? r) (str "expected the error to be raised, got " r))
        (is (re-find #"PathNotAllowed" (second r)))))))

(deftest acknowledgements-must-not-pile-up-unread
  (testing "a transport whose answers nobody collects stops the whole run"
    ;; The buffers here are deliberately tiny, because that is the shape of the
    (let [to-server (PipedOutputStream.)
          from-server (PipedInputStream. 128)
          server-in (PipedInputStream. to-server 4096)
          server-out (PipedOutputStream. from-server)
          conn (remote/->Conn to-server from-server (atom 0) (atom (byte-array 0))
                              (java.util.HashSet.) (fn [] (.close to-server) 0))
          served (atom 0)]
      (future
        (let [carry (ByteArrayOutputStream.)]
          (loop []
            (when-let [req (read-request server-in carry)]
              (.write server-out ^bytes (pack/pack {"i" (:i req) "r" nil}))
              (.flush server-out)
              (swap! served inc)
              (recur)))))
      (let [done (future
                   (dotimes [i 40]
                     (remote/put! conn (byte-array 32 (byte i)) (byte-array 1024 (byte i))))
                   (remote/drain-puts! conn)
                   :done)]
        (is (= :done (deref done 15000 :timed-out))
            "a client that never collects its acknowledgements deadlocks here")
        (is (= 40 @served))
        (is (.isEmpty ^java.util.Set (:pending conn)))))))

