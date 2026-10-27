(ns borgclj.ui.repo
  "What the pages need to read out of a repository. The interface runs in the
   same binary as the verbs, so it asks the archive directly instead of reading
   a verb's printed output back."
  (:require [borgclj.archive :as arch]
            [borgclj.key :as key]
            [borgclj.log :as log]
            [borgclj.ntfs :as ntfs]
            [borgclj.restore :as restore]
            [borgclj.store :as store]
            [borgclj.ui.config :as config]))

(defn- with-store [cfg f]
  (let [k (key/plaintext-key)
        st (store/open (config/location cfg nil) (config/store-opts cfg))]
    (try (f st k (arch/manifest st k)) (finally (store/close st)))))

(defn archive-names [cfg]
  (with-store cfg (fn [_ _ m] (vec (arch/archive-names m)))))

(defn entries
  "One directory inside an image in an archive."
  [cfg {:keys [archive image partition path]}]
  (with-store cfg
    (fn [r k m]
      (let [items (arch/items r k (arch/archive r k m archive))
            where (restore/open-image-volume r k items (not-empty image)
                                             (some-> (not-empty partition) parse-long))
            vol (:vol where)
            hit (or (ntfs/lookup vol path)
                    (throw (ex-info (str "Nicht in diesem Abbild: " path) {})))]
        (mapv (fn [e] {:name (:name e)
                       :dir? (:dir? e)
                       :size (when-not (:dir? e) (log/human (long (:size e))))})
              (ntfs/dir-entries vol (long (:frn hit))))))))
