(ns borgclj.archive
  (:require [borgclj.bytes :as b]
            [borgclj.key :as key]
            [borgclj.msgpack :as mp]
            [borgclj.platform :as plat]
            [borgclj.store :as store])
  (:import [java.nio.file Files LinkOption Path StandardOpenOption]))

(def manifest-id (byte-array 32))

(defn manifest [r k]
  (mp/unpack (key/decrypt k nil (store/get-object r manifest-id))))

(defn archive-names [m]
  (sort (map (comp b/display b/key->str) (keys (:archives m)))))

(defn archive [r k m ^String nm]
  (let [entry (get (into {} (map (fn [[a v]] [(b/key->str a) v])) (:archives m)) nm)]
    (when-not entry (throw (ex-info "archive not found" {:name nm})))
    (let [id (:id entry)]
      (mp/unpack (key/decrypt k id (store/get-object r id))))))

(defn items [r k a]
  ;; The item metadata stream is content-chunked, so a single item's msgpack map
  (let [bos (java.io.ByteArrayOutputStream.)]
    (doseq [id (:items a)]
      (.write bos ^bytes (key/decrypt k id (store/get-object r id))))
    (mp/unpack-stream (.toByteArray bos))))

(defn item-size ^long [it]
  (reduce + 0 (map second (:chunks it))))

(defn- ftype [it]
  (let [m (long (:mode it 0))]
    (cond
      (:source it) :symlink
      (= 0x4000 (bit-and m 0xf000)) :dir
      (= 0x8000 (bit-and m 0xf000)) :file
      :else :other)))

(defn describe [it]
  {:path (b/display (:path it))
   :type (ftype it)
   :mode (:mode it)
   :size (when (= :file (ftype it)) (item-size it))
   :target (some-> (:source it) b/display)})

(defn chunk-bytes ^bytes [r k [id _ _]]
  (key/decrypt k id (store/get-object r id)))

(defn extract-item! [r k ^Path dest it]
  (let [rel (b/display (:path it))
        out (.resolve dest rel)]
    (case (ftype it)
      :dir (Files/createDirectories out (into-array java.nio.file.attribute.FileAttribute []))
      :symlink (do (Files/createDirectories (.getParent out) (into-array java.nio.file.attribute.FileAttribute []))
                   (Files/createSymbolicLink out (Path/of (b/display (:source it)) (into-array String []))
                                             (into-array java.nio.file.attribute.FileAttribute [])))
      :file (do (Files/createDirectories (.getParent out) (into-array java.nio.file.attribute.FileAttribute []))
                (with-open [os (Files/newOutputStream out (into-array StandardOpenOption
                                                                      [StandardOpenOption/CREATE
                                                                       StandardOpenOption/TRUNCATE_EXISTING
                                                                       StandardOpenOption/WRITE]))]
                  (doseq [c (:chunks it)]
                    (.write os (chunk-bytes r k c))))
                (plat/apply-metadata! out it))
      nil)
    out))
