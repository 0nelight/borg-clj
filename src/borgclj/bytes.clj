(ns borgclj.bytes
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(defn latin1 ^String [b]
  (if (string? b)
    b
    (String. ^bytes b StandardCharsets/ISO_8859_1)))

(defn unlatin1 ^bytes [^String s]
  (.getBytes s StandardCharsets/ISO_8859_1))

(defn display ^String [^String latin]
  (String. (unlatin1 latin) StandardCharsets/UTF_8))

(defn sha256 ^bytes [^bytes b]
  (.digest (MessageDigest/getInstance "SHA-256") b))

(defn key->str ^String [k]
  (cond
    (keyword? k) (subs (str k) 1)
    (string? k) k
    (bytes? k) (latin1 k)
    :else (str k)))

(defn ->bytes ^bytes [x]
  (cond
    (bytes? x) x
    (string? x) (unlatin1 x)
    :else (throw (ex-info "not bytes" {:type (type x)}))))

(defn hex ^String [b0]
  (let [^bytes b (->bytes b0)]
    (let [sb (StringBuilder.)]
      (doseq [x b] (.append sb (format "%02x" (bit-and x 0xff))))
      (str sb))))

(defn unhex ^bytes [^String s]
  (let [n (/ (count s) 2)
        out (byte-array n)]
    (dotimes [i n]
      (aset out i (unchecked-byte (Integer/parseInt (subs s (* 2 i) (+ 2 (* 2 i))) 16))))
    out))
