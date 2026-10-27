(ns borgclj.remote
  (:require [borgclj.bytes :as b]
            [borgclj.ssh :as ssh]
            [borgclj.mpack :as pack]
            [borgclj.msgpack :as mp]
            [clojure.string :as str])
  (:import [java.io BufferedInputStream BufferedOutputStream]))

(def ^:const protocol-version 2)
(def borg-version [1 4 5])

(def ^:const max-inflight
  "Unacknowledged puts allowed on the wire. borg's own RemoteRepository keeps the
   same number (MAX_INFLIGHT in borg/remote.py)."
  100)

(def ^:const max-data-size
  "The largest object borg serve stores (MAX_DATA_SIZE in borg/constants.py).
   One byte more and the put comes back as an IntegrityError."
  20971479)

(defn- kbin ^bytes [^String s] (b/unlatin1 s))

(defrecord Conn [^java.io.OutputStream out ^java.io.InputStream in msgid buf pending closer])

(defn spawn [argv]
  (let [pb (doto (ProcessBuilder. ^java.util.List (vec argv))
             (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))
        p (.start pb)]
    (->Conn (BufferedOutputStream. (.getOutputStream p))
            (BufferedInputStream. (.getInputStream p)) (atom 0) (atom (byte-array 0))
            (java.util.HashSet.)
            ;; A bounded wait, then force it. `.waitFor` without one hangs the
            (fn []
              (try (.close (.getOutputStream p)) (catch Exception _ nil))
              (when-not (.waitFor p 3 java.util.concurrent.TimeUnit/SECONDS)
                (.destroy p)
                (when-not (.waitFor p 2 java.util.concurrent.TimeUnit/SECONDS)
                  (.destroyForcibly p)))
              (.exitValue p)))))

(defn connect-ssh [opts]
  (let [{:keys [in out close]} (ssh/exec opts)]
    (->Conn (BufferedOutputStream. out) (BufferedInputStream. in)
            (atom 0) (atom (byte-array 0)) (java.util.HashSet.) close)))

(defn- read-one [conn]
  (let [^java.io.InputStream in (:in conn)
        buf (:buf conn)
        tmp (byte-array 262144)]
    (loop []
      (let [^bytes data @buf
            parsed (when (pos? (alength data))
                     (try (mp/read-val data 0) (catch Exception _ nil)))]
        (if parsed
          (let [[v off] parsed]
            (reset! buf (java.util.Arrays/copyOfRange data (int off) (alength data)))
            v)
          (let [got (.read in tmp 0 (alength tmp))]
            (when (neg? got) (throw (ex-info "connection closed by borg serve" {})))
            (let [^bytes cur @buf
                  merged (byte-array (+ (alength cur) got))]
              (System/arraycopy cur 0 merged 0 (alength cur))
              (System/arraycopy tmp 0 merged (alength cur) got)
              (reset! buf merged))
            (recur)))))))

(defn- norm [m]
  (into {} (map (fn [[k v]]
                  [(cond (bytes? k) (keyword (b/latin1 k))
                         (keyword? k) k
                         :else (keyword (str k)))
                   v])
                m)))

(defn- take-response!
  "Reads until borg serve answers a request, stepping over its log chatter."
  [conn]
  (loop []
    (let [resp0 (read-one conn)
          resp (if (map? resp0) (norm resp0) resp0)]
      (cond
        (and (map? resp) (contains? resp :r)) [(:i resp) (:r resp)]

        (and (map? resp) (contains? resp :exception_class))
        (throw (ex-info (str "remote: " (b/latin1 (:exception_class resp)) ": "
                             (str/trim (str/join " " (map b/latin1 (:exception_short resp)))))
                        {:remote true}))

        (and (vector? resp) (= 4 (count resp)))
        (throw (ex-info (str "remote: " (nth resp 2) ": " (nth resp 3)) {:remote true}))

        :else (recur)))))

(defn- ack1! [conn ^java.util.Set pend]
  (let [[msgid _] (take-response! conn)]
    (when-not (.remove pend msgid)
      (throw (ex-info "unexpected message id from borg serve" {:msgid msgid})))))

(defn drain-puts!
  "Waits until every outstanding put has been acknowledged."
  [conn]
  (let [^java.util.Set pend (:pending conn)]
    (when-not (.isEmpty pend)
      (.flush ^java.io.OutputStream (:out conn))
      (while (not (.isEmpty pend)) (ack1! conn pend)))))

(defn call [conn ^String method args]
  (drain-puts! conn)
  (let [id (swap! (:msgid conn) inc)
        req {(kbin "i") id (kbin "m") (kbin method) (kbin "a") args}
        ^java.io.OutputStream out (:out conn)]
    (.write out ^bytes (pack/pack req))
    (.flush out)
    (second (take-response! conn))))

(defn negotiate [conn]
  (call conn "negotiate" {"client_data" {"client_version" borg-version}}))

(defn- open-call! [conn path {:keys [create lock exclusive append-only make-parent-dirs]}]
  (b/->bytes (call conn "open" {"path" path "create" create "lock_wait" 10 "lock" lock
                                "exclusive" exclusive "append_only" append-only
                                "make_parent_dirs" make-parent-dirs})))

(defn open!
  "Opens the repository, creating it when :create is set and it is not there
   yet."
  [conn path & {:keys [create lock exclusive append-only make-parent-dirs]
                :or {create false lock true exclusive nil append-only false make-parent-dirs false}}]
  (negotiate conn)
  (let [args {:create create :lock lock :exclusive exclusive
              :append-only append-only :make-parent-dirs make-parent-dirs}]
    (try
      (open-call! conn path args)
      (catch clojure.lang.ExceptionInfo e
        (if create
          (try (open-call! conn path (assoc args :create false))
               (catch clojure.lang.ExceptionInfo _ (throw e)))
          (throw e))))))

(defn- drain-ready!
  "Collects the acknowledgements that have already come back, without waiting
   for any that have not."
  [conn ^java.util.Set pend]
  (let [^java.io.InputStream in (:in conn)
        buf (:buf conn)]
    (while (and (not (.isEmpty pend))
                (or (pos? (alength ^bytes @buf)) (pos? (.available in))))
      (ack1! conn pend))))

(defn put!
  "Hands one object to the repository without waiting for its acknowledgement,
   so borg serve stores one chunk while the client already hashes and
   compresses the next."
  [conn ^bytes id ^bytes data]
  (let [^java.io.OutputStream out (:out conn)
        ^java.util.Set pend (:pending conn)
        msgid (swap! (:msgid conn) inc)]
    (.write out ^bytes (pack/pack {(kbin "i") msgid (kbin "m") (kbin "put")
                                   (kbin "a") {(kbin "id") id (kbin "data") data}}))
    (.flush out)
    (.add pend msgid)
    (drain-ready! conn pend)
    (when (>= (.size pend) max-inflight)
      (ack1! conn pend))))

(defn get-object ^bytes [conn ^bytes id]
  (b/->bytes (call conn "get" {"id" id})))

(defn get-pipelined!
  "Ask for every id and hand the envelopes to (f id bytes) as they come back."
  [conn ids f & [{:keys [window] :or {window 64}}]]
  (let [^java.io.OutputStream out (:out conn)
        pend (java.util.HashMap.)
        send! (fn [id]
                (let [msgid (swap! (:msgid conn) inc)]
                  (.write out ^bytes (pack/pack {(kbin "i") msgid (kbin "m") (kbin "get")
                                                 (kbin "a") {(kbin "id") (b/->bytes id)}}))
                  (.put pend msgid id)))
        take1! (fn []
                 (let [[msgid r] (take-response! conn)
                       id (.remove pend msgid)]
                   (when-not id (throw (ex-info "unexpected message id from borg serve"
                                                {:msgid msgid})))
                   (f id (b/->bytes r))))]
    (drain-puts! conn)
    (loop [todo (seq ids)]
      (when todo
        (send! (first todo))
        (when (>= (.size pend) (long window)) (.flush out) (take1!))
        (recur (next todo))))
    (.flush out)
    (while (pos? (.size pend)) (take1!))))

(defn commit! [conn]
  (call conn "commit" {"save_space" false "compact" false}))

(defn rollback! [conn]
  (call conn "rollback" {}))

(defn ids [conn]
  (loop [marker nil acc []]
    (let [batch (call conn "list" (cond-> {"limit" 10000} marker (assoc "marker" marker)))]
      (if (empty? batch)
        acc
        (let [bs (mapv b/->bytes batch)]
          (recur (last bs) (into acc bs)))))))

(defn close [conn]
  (try (.flush ^java.io.OutputStream (:out conn)) (catch Exception _ nil))
  ((:closer conn)))

(defn parse-location [^String loc]
  (cond
    (str/starts-with? loc "ssh://")
    (let [rest- (subs loc 6)
          [hostpart & pathparts] (str/split rest- #"/")
          [user hostport] (if (str/includes? hostpart "@")
                            (str/split hostpart #"@" 2)
                            [nil hostpart])
          [host port] (str/split hostport #":" 2)]
      {:user user :host host :port (if port (parse-long port) 22)
       :path (str "/" (str/join "/" pathparts))})
    (re-find #"^([^/@]+@)?[^/:]+:" loc)
    (let [[hostpart path] (str/split loc #":" 2)
          [user host] (if (str/includes? hostpart "@")
                        (str/split hostpart #"@" 2)
                        [nil hostpart])]
      {:user user :host host :port 22 :path path})
    :else nil))

(defn location->argv [^String loc {:keys [server-cmd rsh remote-path]}]
  (cond
    server-cmd {:argv (str/split server-cmd #"\s+") :path loc}
    (str/starts-with? loc "ssh://")
    (let [rest- (subs loc 6)
          [hostpart & pathparts] (str/split rest- #"/")
          path (str "/" (str/join "/" pathparts))]
      {:argv (concat (str/split (or rsh "ssh") #"\s+") [hostpart (or remote-path "borg") "serve"]) :path path})
    (re-find #"^[^/]+@[^/:]+:" loc)
    (let [[hostpart path] (str/split loc #":" 2)]
      {:argv (concat (str/split (or rsh "ssh") #"\s+") [hostpart (or remote-path "borg") "serve"]) :path path})
    :else nil))
