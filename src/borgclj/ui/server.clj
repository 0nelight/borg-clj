(ns borgclj.ui.server
  "A local web interface: the same verbs the command line offers, driven from a
   browser. It listens on 127.0.0.1 only and answers nobody who does not bring
   the one-time token out of the URL it printed."
  (:require [borgclj.bytes :as b]
            [borgclj.ui.config :as config]
            [borgclj.ui.html :as h]
            [borgclj.ui.job :as job]
            [borgclj.ui.pages :as pages]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io StringWriter]
           [java.net InetSocketAddress URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent Executors]))

(def ^:private cookie-name "borgui")

(def ^:private asset
  (delay {"/app.css" ["text/css; charset=utf-8" (slurp (io/resource "app.css"))]
          "/app.js" ["application/javascript; charset=utf-8" (slurp (io/resource "app.js"))]}))

(defn new-token
  "Guards against another local process wandering in -- not against someone who
   can already run code as this user, who could read the cookie anyway."
  ^String []
  (let [buf (byte-array 16)]
    (.nextBytes (java.security.SecureRandom.) buf)
    (b/hex buf)))

(defn- decode ^String [^String s] (URLDecoder/decode s StandardCharsets/UTF_8))

(defn- pairs [s]
  (vec (for [kv (str/split (or s "") #"&")
             :when (seq kv)
             :let [[k v] (str/split kv #"=" 2)]]
         [(decode k) (decode (or v ""))])))

(defn- request [^HttpExchange ex]
  (let [uri (.getRequestURI ex)
        body (when (= "POST" (.getRequestMethod ex))
               (String. (.readAllBytes (.getRequestBody ex)) StandardCharsets/UTF_8))
        query (pairs (.getRawQuery uri))
        form (pairs body)
        all (into form query)]
    {:method (.getRequestMethod ex)
     :path (.getPath uri)
     ;; form first, then the query -- a POST page may carry both
     :param (fn [n] (or (some (fn [[k v]] (when (= k n) v)) all) ""))
     :params (fn [n] (keep (fn [[k v]] (when (= k n) v)) all))
     :query (fn [n] (or (some (fn [[k v]] (when (= k n) v)) query) ""))
     :cookie (fn [n] (some (fn [c] (let [[k v] (str/split (str/trim c) #"=" 2)]
                                     (when (= k n) v)))
                           (str/split (or (.getFirst (.getRequestHeaders ex) "Cookie") "") #";")))}))

(defn- send! [^HttpExchange ex ^long code ^String ctype ^String body]
  (let [bs (.getBytes body "UTF-8")]
    (.set (.getResponseHeaders ex) "Content-Type" ctype)
    (.sendResponseHeaders ex code (alength bs))
    (with-open [os (.getResponseBody ex)] (.write os bs))))

(defn- html! [ex body] (send! ex 200 "text/html; charset=utf-8" body))

(defn- redirect! [^HttpExchange ex ^String to]
  (.set (.getResponseHeaders ex) "Location" to)
  (.sendResponseHeaders ex 302 -1)
  (.close ex))

(defn- events!
  "The progress stream app.js listens on. Without it the run page carries a
   meta-refresh instead and the server renders the same numbers."
  [app ^HttpExchange ex]
  (.set (.getResponseHeaders ex) "Content-Type" "text/event-stream; charset=utf-8")
  (.set (.getResponseHeaders ex) "Cache-Control" "no-cache")
  (.sendResponseHeaders ex 200 0)
  (with-open [os (.getResponseBody ex)]
    ;; a closed tab is the ordinary way this ends
    (let [push (fn [^String s] (.write os (.getBytes s "UTF-8")) (.flush os))]
      (push "retry: 2000\n\n")
      (loop [seen -1 idx 0 idle 0 ran? false]
        (let [s (job/snapshot (:job app))]
          (if (= (:seq s) seen)
            (do
              ;; a comment every ten seconds: it costs nothing and it is how a
              ;; dead peer is noticed when nothing else is being sent
              (when (>= idle 40) (push ": .\n\n"))
              (Thread/sleep 250)
              (recur seen idx (if (>= idle 40) 0 (inc idle)) ran?))
            (let [[lines next-idx] (job/log-from s idx)]
              (push (str "event: zustand\ndata: " (pages/event-payload s lines) "\n\n"))
              (when-not (and ran? (not= :running (:state s)))
                (Thread/sleep 250)
                (recur (:seq s) next-idx 0
                       (or ran? (= :running (:state s))))))))))))

(defn- route [app req ex]
  (let [p (:path req)
        post? (= "POST" (:method req))
        page (fn [f] (html! ex (f app req)))
        act (fn [f] (let [r (f app req)]
                      (if (:redirect r) (redirect! ex (:redirect r)) (html! ex r))))]
    (case p
      ("/" "") (redirect! ex "/uebersicht")
      "/ereignisse" (events! app ex)
      "/uebersicht" (do (when (= "1" ((:query req) "neu")) (pages/disks app true))
                        (page pages/overview))
      "/ziel" (if post? (act pages/post-target) (page pages/target))
      "/sichern" (if post? (act pages/post-backup) (page pages/backup))
      "/sicherungen" (page pages/archives)
      "/dateien" (if post? (act pages/post-fetch) (page pages/files))
      "/zurueck" (if post? (act pages/post-restore) (page pages/restore))
      "/lauf" (page pages/run-page)
      "/abbrechen" (do (job/stop! (:job app)) (redirect! ex "/lauf"))
      (if-let [[ctype body] (get @asset p)]
        (send! ex 200 ctype body)
        (send! ex 404 "text/html; charset=utf-8"
               (str "<!doctype html><meta charset=\"utf-8\"><p>Diese Seite gibt es nicht. "
                    "<a href=\"/uebersicht\">Zur Übersicht</a>"))))))

(defn- failed! [^HttpExchange ex ^Throwable t]
  ;; a browser left waiting on an exchange nobody answered is worse than a page
  ;; that says what went wrong
  (when (neg? (.getResponseCode ex))
    (send! ex 500 "text/html; charset=utf-8"
           (h/layout {:active "" :title "Fehler" :head ""
                      :subtitle "Die Anfrage ist gescheitert."
                      :body (str "<div class=\"warn\"><pre class=\"log\">"
                                 (h/esc (or (ex-message t) (.getName (class t))))
                                 "</pre></div>")}))))

(defn- dispatch* [app ^HttpExchange ex]
  (let [req (request ex)
        token (:token app)]
    ;; the token arrives once in the URL the program opened, then lives in a cookie
    (cond
      (= token ((:query req) "t"))
      (do (.set (.getResponseHeaders ex) "Set-Cookie"
                (str cookie-name "=" token "; Path=/; HttpOnly; SameSite=Strict"))
          (route app req ex))

      (= token ((:cookie req) cookie-name))
      (route app req ex)

      :else
      (send! ex 403 "text/html; charset=utf-8"
             (str "<!doctype html><meta charset=\"utf-8\"><title>abgewiesen</title>"
                  "<body style=\"font:14px sans-serif;padding:40px\">"
                  "<h2>Nicht berechtigt</h2><p>Diese Oberfläche nimmt nur Anfragen an, die das "
                  "Einmal-Kennwort dieses Programmlaufs mitbringen. Bitte den Verweis benutzen, "
                  "den das Programm beim Start ausgegeben oder geöffnet hat.</p>")))))

(defn- dispatch [app ^HttpExchange ex]
  (try (dispatch* app ex)
       (catch java.io.IOException _ nil)
       (catch Throwable t (failed! ex t))))

(defn- bind ^HttpServer [^long port]
  (loop [p port n 0]
    (if-let [s (try (HttpServer/create (InetSocketAddress. "127.0.0.1" (int p)) 16)
                    (catch java.io.IOException _ nil))]
      s
      (if (< n 40)
        (recur (inc p) (inc n))
        (throw (ex-info (str "kein freier Port ab " port " gefunden") {:port port}))))))

(defn- open-browser [^String url]
  (let [os (System/getProperty "os.name")
        argv (cond
               ;; `start` is a cmd builtin, not a program; the empty string is
               ;; the window title cmd would otherwise take the URL for
               (str/starts-with? os "Windows") ["cmd" "/c" "start" "" url]
               (str/starts-with? os "Mac") ["open" url]
               :else ["xdg-open" url])]
    (try (.start (ProcessBuilder. ^java.util.List argv))
         ;; no browser is not fatal -- the URL is printed and can be written to a file
         (catch java.io.IOException _ nil))))

(defn- capture
  "A read-only verb's output as text, for the panels that show it verbatim."
  [run]
  (fn [argv]
    (let [w (StringWriter.)]
      (binding [*out* w *err* w]
        (try (run argv) (catch Throwable t (println (or (ex-message t) (str (class t)))))))
      (str w))))

(defn serve!
  "Starts the interface and hands back what it is serving; the caller keeps the
   process alive."
  [{:keys [port no-browser url-file run]}]
  ;; Der Typ muss hier stehen, nicht nur an bind: dessen ^long-Parameter zwingt
  ;; den Aufruf auf den geboxten Weg, und dabei geht der Rueckgabe-Typ verloren.
  ;; Ohne die Angabe wird .getAddress reflektiv aufgeloest -- auf der JVM faellt
  ;; das nicht auf, im native-image scheitert es an sun.net.httpserver.HttpServerImpl.
  (let [^HttpServer server (bind (or port 8722))
        app {:job (job/new-job)
             :config (atom (config/load-config))
             :disks (atom nil)
             :run run
             :capture (capture run)
             :token (new-token)}
        url (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/?t=" (:token app))]
    (.createContext server "/" (reify HttpHandler (handle [_ ex] (dispatch app ex))))
    (.setExecutor server (Executors/newCachedThreadPool))
    (.start server)
    (println url)
    (flush)
    (when url-file (spit url-file (str url "\n")))
    (when-not no-browser (open-browser url))
    {:server server :app app :url url}))
