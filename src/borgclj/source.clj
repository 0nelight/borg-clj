(ns borgclj.source
  (:import [java.nio.channels FileChannel]
           [java.nio ByteBuffer]
           [java.nio.file Path StandardOpenOption]
           [java.util Arrays]))

(defprotocol Source
  (s-name [_])
  (s-length [_])
  (s-sector [_])
  (s-extents [_])
  (s-cuts [_])
  (s-read [_ off dst len])
  (s-info [_])
  (s-close [_]))

(defn prune-cuts
  "Drops cut points that would leave a segment shorter than min-seg, so a
   fragmented volume cannot explode the chunk count."
  [cuts ^long min-seg ^long length]
  (->> cuts
       (map long)
       (filter #(and (pos? %) (< % length)))
       distinct
       sort
       (reduce (fn [acc c]
                 (if (and (seq acc) (< (- (long c) (long (peek acc))) min-seg))
                   acc
                   (conj acc c)))
               [])
       vec))

(defn- align-down ^long [^long v ^long a] (* a (quot v a)))
(defn- align-up ^long [^long v ^long a] (* a (quot (+ v (dec a)) a)))

(defn normalize
  "Sorts, sector-aligns and merges [off len] extents, clamped to length."
  [extents ^long length ^long sector]
  (let [clean (->> extents
                   (keep (fn [[o l]]
                           (let [start (max 0 (align-down (long o) sector))
                                 e (min length (align-up (+ (long o) (long l)) sector))]
                             (when (> e start) [start (- e start)]))))
                   (sort-by first))]
    (persistent!
      (reduce (fn [acc [o l]]
                (let [n (count acc)
                      [po pl] (when (pos? n) (nth acc (dec n)))]
                  (if (and po (<= (long o) (+ (long po) (long pl))))
                    (assoc! acc (dec n) [po (- (max (+ (long po) (long pl)) (+ (long o) (long l))) (long po))])
                    (conj! acc [o l]))))
              (transient []) clean))))

(defn runs->extents [runs ^long unit]
  (mapv (fn [[start n]] [(* (long start) unit) (* (long n) unit)]) runs))

(defn subtract
  "Normalized extents a with every byte covered by b removed."
  [a b ^long length ^long sector]
  (if (empty? b)
    (vec a)
    (let [holes (normalize b length sector)]
      (normalize
        (reduce (fn [acc [o l]]
                  (let [o (long o) e (+ o (long l))]
                    (into acc
                          (loop [at o [[ho hl] & more] holes out []]
                            (cond
                              (nil? ho) (if (< at e) (conj out [at (- e at)]) out)
                              (>= (long ho) e) (if (< at e) (conj out [at (- e at)]) out)
                              (<= (+ (long ho) (long hl)) at) (recur at more out)
                              :else (let [hs (max at (long ho))
                                          he (min e (+ (long ho) (long hl)))]
                                      (recur (max at he) more
                                             (if (< at hs) (conj out [at (- hs at)]) out))))))))
                [] a)
        length sector))))

(defn total ^long [extents]
  (reduce (fn [^long a [_ l]] (+ a (long l))) 0 extents))

(defn index
  "Binary-searchable form of a normalized extent list."
  [extents]
  (let [n (count extents)
        starts (long-array n)
        ends (long-array n)]
    (dotimes [i n]
      (let [[o l] (nth extents i)]
        (aset starts i (long o))
        (aset ends i (+ (long o) (long l)))))
    {:starts starts :ends ends :n n}))

(defn covering
  "The parts of [off off+len) that fall inside the indexed extents, as [o l] pairs."
  [{:keys [^longs starts ^longs ends ^long n]} ^long off ^long len]
  (when (pos? n)
    (let [stop (+ off len)
          i0 (let [p (Arrays/binarySearch starts off)]
               (if (>= p 0) p (max 0 (dec (- (- p) 1)))))]
      (loop [i i0 acc []]
        (if (or (>= i n) (>= (aget starts i) stop))
          acc
          (let [o (max off (aget starts i))
                e (min stop (aget ends i))]
            (recur (inc i) (if (> e o) (conj acc [o (- e o)]) acc))))))))

(defrecord FileSource [^FileChannel ch ^String name ^long length ^long sector extents cuts info]
  Source
  (s-name [_] name)
  (s-length [_] length)
  (s-sector [_] sector)
  (s-extents [_] extents)
  (s-cuts [_] cuts)
  (s-read [_ off dst len]
    (let [bb (ByteBuffer/wrap dst 0 (int len))]
      (loop [pos (long off)]
        (when (pos? (.remaining bb))
          (let [n (.read ch bb pos)]
            (when (neg? n) (throw (ex-info "short read" {:offset off})))
            (recur (+ pos n)))))
      len))
  (s-info [_] info)
  (s-close [_] (.close ch)))

(defn file-source
  ([^Path p] (file-source p nil nil))
  ([^Path p extents] (file-source p extents nil))
  ([^Path p extents cuts]
   (let [ch (FileChannel/open p (into-array StandardOpenOption [StandardOpenOption/READ]))
         len (.size ch)
         sector 512]
     (->FileSource ch (str (.getFileName p)) len sector
                   (normalize (or extents [[0 len]]) len sector)
                   (prune-cuts (or cuts []) sector len)
                   {:kind :file :path (str p) :length len}))))

(defrecord CompositeSource [^String name ^long length ^long sector extents cuts parts info]
  Source
  (s-name [_] name)
  (s-length [_] length)
  (s-sector [_] sector)
  (s-extents [_] extents)
  (s-cuts [_] cuts)
  (s-read [_ off dst len]
    (loop [at (long off) left (long len) into 0]
      (if-not (pos? left)
        len
        (let [p (or (some (fn [{:keys [^long base ^long size] :as p}]
                            (when (and (>= at base) (< at (+ base size))) p))
                          parts)
                    (throw (ex-info "offset outside every part" {:offset at})))
              take- (min left (- (+ (long (:base p)) (long (:size p))) at))
              tmp (if (zero? into) dst (byte-array take-))]
          (s-read (:src p) (+ (long (:inner p)) (- at (long (:base p)))) tmp take-)
          (when (pos? into) (System/arraycopy tmp 0 dst into (int take-)))
          (recur (+ at take-) (- left take-) (+ into take-))))))
  (s-info [_] info)
  (s-close [_] (doseq [x (distinct (keep (fn [p] (when (:own p) (:src p))) parts))] (s-close x))))

(defn boundaries
  "Offsets at which a composite source must not be read across."
  [parts]
  (into (sorted-set) (mapcat (fn [{:keys [^long base ^long size]}] [base (+ base size)]) parts)))
