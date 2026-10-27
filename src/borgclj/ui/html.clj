(ns borgclj.ui.html
  (:require [borgclj.log :as log]
            [clojure.string :as str]))

(defn esc ^String [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn url-enc ^String [s]
  (java.net.URLEncoder/encode (str s) "UTF-8"))

(defn duration ^String [secs]
  (if (or (nil? secs) (neg? (long secs)))
    "--:--"
    (let [s (long secs) h (quot s 3600)]
      (if (pos? h)
        (log/fmt "%d:%02d:%02d" h (quot (mod s 3600) 60) (mod s 60))
        (log/fmt "%02d:%02d" (quot s 60) (mod s 60))))))

(defn percent
  "With a comma, because app.js writes one and the same number must not change
   shape the moment the browser takes over."
  ^String [v]
  (str (str/replace (log/fmt "%.1f" (double v)) "." ",") " %"))

(defn rate ^String [bytes-per-second]
  (if bytes-per-second (str (log/human (long bytes-per-second)) "/s") "--"))

(defn json ^String [x]
  (cond
    (nil? x) "null"
    (string? x) (str \" (-> x
                            (str/replace "\\" "\\\\")
                            (str/replace "\"" "\\\"")
                            (str/replace "\n" "\\n")
                            (str/replace "\r" "\\r")
                            (str/replace "\t" "\\t"))
                     \")
    (or (true? x) (false? x)) (str x)
    (number? x) (str x)
    (map? x) (str "{" (str/join "," (map (fn [[k v]] (str (json (name k)) ":" (json v))) x)) "}")
    :else (str "[" (str/join "," (map json x)) "]")))

(def ^:private items
  [["/uebersicht" "Übersicht"]
   ["/ziel" "Ziel"]
   ["/sichern" "Sichern"]
   ["/sicherungen" "Sicherungen"]
   ["/dateien" "Dateien holen"]
   ["/zurueck" "Zurückspielen"]])

(defn layout
  "A list, not a row of bare links: a text browser runs inline anchors together
   into one unreadable word, and <li> gives it a line to break on while CSS
   still renders them as blocks."
  ^String [{:keys [active title subtitle head body]}]
  (str "<!doctype html><html lang=\"de\"><head><meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
       "<title>" (esc title) " — borg-imager</title>"
       "<link rel=\"stylesheet\" href=\"/app.css\">" head "</head><body>"
       "<nav><h1>borg-imager</h1><p>Abbildsicherung</p><ul>"
       (apply str (for [[href caption] items]
                    (str "<li><a href=\"" href "\""
                         (when (= active href) " class=\"on\"") ">" caption "</a></li>")))
       "</ul></nav><main><h2>" (esc title) "</h2>"
       "<p class=\"sub\">" (esc subtitle) "</p>"
       body
       "</main></body></html>"))
