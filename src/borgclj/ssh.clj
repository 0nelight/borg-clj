(ns borgclj.ssh
  (:import [com.jcraft.jsch ChannelExec JSch Session]))

(defn- identity-files [explicit]
  (if explicit
    [explicit]
    (let [home (System/getProperty "user.home")]
      (filterv #(.exists (java.io.File. ^String %))
               (map #(str home java.io.File/separator ".ssh" java.io.File/separator %)
                    ["id_ed25519" "id_rsa" "id_ecdsa"])))))

(defn exec
  [{:keys [host user port command identity strict-host-key-checking timeout]
    :or {port 22 strict-host-key-checking "no" timeout 15000}}]
  (let [jsch (JSch.)
        _ (doseq [f (identity-files identity)] (.addIdentity jsch ^String f))
        session (.getSession jsch user host (int port))]
    (.setConfig session "StrictHostKeyChecking" strict-host-key-checking)
    (.setConfig session "PreferredAuthentications" "publickey")
    (when (= "no" strict-host-key-checking)
      (.setConfig session "HashKnownHosts" "no"))
    (.connect session (int timeout))
    (let [^ChannelExec ch (.openChannel session "exec")]
      (.setCommand ch ^String command)
      ;; dontclose=true: with the one-argument form JSch owns the stream and
      (.setErrStream ch System/err true)
      (let [in (.getInputStream ch)
            out (.getOutputStream ch)]
        (.connect ch (int timeout))
        {:in in :out out
         :close (fn []
                  (try (.close out) (catch Exception _ nil))
                  (try (.disconnect ch) (catch Exception _ nil))
                  (.disconnect session)
                  0)}))))
