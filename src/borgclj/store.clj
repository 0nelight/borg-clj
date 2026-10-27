(ns borgclj.store
  (:require [borgclj.bytes :as b]
            [borgclj.remote :as remote]
            [borgclj.repository :as repo]
            [clojure.string :as str]))

(defprotocol Store
  (get-object [_ id])
  (get-objects [_ ids f] "call (f id bytes) for every id, in whatever order they arrive")
  (list-ids [_])
  (describe [_])
  (close [_]))

(defrecord LocalStore [repo]
  Store
  (get-object [_ id]
    (if-let [loc (get (:idx repo) (b/hex id))]
      (repo/read-object loc)
      (throw (ex-info "object not found" {:id (b/hex id)}))))
  (get-objects [this ids f] (doseq [id ids] (f id (get-object this id))))
  (list-ids [_] (mapv b/unhex (keys (:idx repo))))
  (describe [_] {:kind :local
                 :path (str (:path repo))
                 :id (get-in repo [:config :id])
                 :objects (count (:idx repo))
                 :segments (count (repo/segments (:path repo)))})
  (close [_] nil))

(defrecord RemoteStore [conn id]
  Store
  (get-object [_ id'] (remote/get-object conn id'))
  (get-objects [_ ids f] (remote/get-pipelined! conn ids f))
  (list-ids [_] (remote/ids conn))
  (describe [_] {:kind :remote :id id})
  (close [_] (remote/close conn)))

(defn local [^String path]
  (->LocalStore (repo/open path)))

(defn remote
  ([conn repo-path] (remote conn repo-path {}))
  ([conn repo-path {:keys [lock] :or {lock true}}]
   (->RemoteStore conn (b/hex (remote/open! conn repo-path :lock lock)))))

(defn connect
  "A transport to whatever the location names: a borg serve spawned here, one
   reached through an external ssh binary, or the built-in ssh client."
  [loc opts]
  (let [remote-path (or (get opts "--remote-path") (System/getenv "BORG_REMOTE_PATH") "borg")
        rsh (or (get opts "--rsh") (System/getenv "BORG_RSH"))]
    (cond
      (get opts "--server-cmd")
      [(remote/spawn (str/split (get opts "--server-cmd") #"\s+")) loc]

      rsh
      (let [{:keys [argv path]} (or (remote/location->argv loc {:rsh rsh :remote-path remote-path})
                                    (throw (ex-info (str "no ssh target in " loc) {})))]
        [(remote/spawn argv) path])

      :else
      (let [{:keys [user host port path]} (or (remote/parse-location loc)
                                              (throw (ex-info (str "no borg serve target in " loc
                                                                   " -- use ssh://host/path, host:path or --server-cmd")
                                                              {})))]
        [(remote/connect-ssh {:host host :port port
                              :user (or user (get opts "--user") (System/getProperty "user.name"))
                              :identity (or (get opts "--identity") (System/getenv "BORG_SSH_KEY"))
                              :command (str remote-path " serve")})
         path]))))

(defn open
  "The store behind a location -- a directory on this machine, or a borg serve
   at the other end of a connection."
  ([loc opts] (open loc opts {}))
  ([loc opts store-opts]
   (if (or (get opts "--server-cmd") (remote/parse-location loc))
     (let [[conn rpath] (connect loc opts)]
       (remote conn rpath store-opts))
     (local loc))))
