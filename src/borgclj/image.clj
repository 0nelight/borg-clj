(ns borgclj.image
  (:require [borgclj.source :as src])
  (:import [java.util Arrays]))

(def ^:const default-block (* 4 1024 1024))

(defn blocks-for
  "Block spans [offset length] covering [0,length)."
  [^long length ^long block-size cuts]
  (let [cs (->> (concat [0] (map long cuts) [length])
                (filter #(<= 0 (long %) length))
                distinct sort vec)]
    (persistent!
      (reduce (fn [acc [a b]]
                (loop [o (long a) acc acc]
                  (if (>= o (long b))
                    acc
                    (recur (+ o block-size)
                           (conj! acc [o (min block-size (- (long b) o))])))))
              (transient []) (partition 2 1 cs)))))

(defn plan [source ^long block-size]
  (let [len (long (src/s-length source))
        ex (src/s-extents source)
        cuts (src/s-cuts source)
        bs (blocks-for len block-size cuts)]
    {:block-size block-size
     :length len
     :blocks bs
     :count (count bs)
     :cuts (count cuts)
     :aligned (boolean (seq cuts))
     :allocated (src/total ex)
     :index (src/index ex)}))

(defn hole?
  "True when no allocated extent touches [off,off+len). Such a block is all
   zeros by definition — there is nothing on the disk to read for it."
  [plan ^long off ^long len]
  (empty? (src/covering (:index plan) off len)))

(defn read-block!
  "Fills buf[0..len) with [off,off+len); anything outside the extents stays
   zero."
  [source plan ^bytes buf off len]
  (let [off (long off)
        len (long len)
        ranges (src/covering (:index plan) off len)]
    (cond
      (empty? ranges) (do (Arrays/fill buf 0 (int len) (byte 0)) [:zero 0])

      (and (= 1 (count ranges))
           (= (long (ffirst ranges)) off)
           (= (long (second (first ranges))) len))
      (do (src/s-read source off buf len) [:full len])

      :else
      (do (Arrays/fill buf 0 (int len) (byte 0))
          (let [tmp (byte-array len)]
            (doseq [[o l] ranges]
              (src/s-read source o tmp l)
              (System/arraycopy tmp 0 buf (int (- (long o) off)) (int l))))
          [:partial (reduce (fn [^long a [_ l]] (+ a (long l))) 0 ranges)]))))

(defn zero-block? [^bytes b ^long len]
  (loop [i 0] (cond (>= i len) true (zero? (aget b (int i))) (recur (inc i)) :else false)))

(defn image-name ^String [source]
  (let [info (src/s-info source)]
    (case (:kind info)
      :volume (str (or (:letter info) "volume") ".img")
      :disk (str "PhysicalDrive" (:disk info) ".img")
      (str (src/s-name source) ".img"))))
