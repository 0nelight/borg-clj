(ns borgclj.msgpack
  (:require [borgclj.bytes :as b])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.util Arrays]))

(declare read-val)

(defn- ubyte ^long [^bytes d ^long i] (bit-and (aget d (int i)) 0xff))

(defn- slice ^bytes [^bytes d ^long off ^long len]
  (when (> (+ off len) (alength d))
    (throw (ex-info "msgpack truncated" {:need (+ off len) :have (alength d)})))
  (Arrays/copyOfRange d (int off) (int (+ off len))))

(defn- u16 ^long [^bytes d ^long i] (bit-or (bit-shift-left (ubyte d i) 8) (ubyte d (inc i))))

(defn- u32 ^long [^bytes d ^long i]
  (bit-or (bit-shift-left (ubyte d i) 24) (bit-shift-left (ubyte d (+ i 1)) 16)
          (bit-shift-left (ubyte d (+ i 2)) 8) (ubyte d (+ i 3))))

(defn- i64 ^long [^bytes d ^long i]
  (.getLong (doto (ByteBuffer/wrap d) (.order ByteOrder/BIG_ENDIAN)) (int i)))

(defn- read-n [^bytes d ^long off ^long n f]
  (loop [i 0 p off acc (transient [])]
    (if (= i n)
      [(persistent! acc) p]
      (let [[v p'] (f d p)]
        (recur (inc i) (long p') (conj! acc v))))))

(defn- read-map [^bytes d ^long off ^long n]
  (loop [i 0 p off acc (transient {})]
    (if (= i n)
      [(persistent! acc) p]
      (let [[k p1] (read-val d p)
            [v p2] (read-val d (long p1))]
        (recur (inc i) (long p2) (assoc! acc (if (string? k) (keyword k) k) v))))))

(defn read-val [^bytes d ^long off]
  (let [t (ubyte d off)
        p (inc off)]
    (cond
      (< t 0x80) [t p]
      (< t 0x90) (read-map d p (bit-and t 0x0f))
      (< t 0xa0) (read-n d p (bit-and t 0x0f) read-val)
      (< t 0xc0) (let [n (bit-and t 0x1f)] [(b/latin1 (slice d p n)) (+ p n)])
      (= t 0xc0) [nil p]
      (= t 0xc2) [false p]
      (= t 0xc3) [true p]
      (= t 0xc4) (let [n (ubyte d p)] [(slice d (inc p) n) (+ p 1 n)])
      (= t 0xc5) (let [n (u16 d p)] [(slice d (+ p 2) n) (+ p 2 n)])
      (= t 0xc6) (let [n (u32 d p)] [(slice d (+ p 4) n) (+ p 4 n)])
      (= t 0xca) [(Float/intBitsToFloat (unchecked-int (u32 d p))) (+ p 4)]
      (= t 0xcb) [(Double/longBitsToDouble (i64 d p)) (+ p 8)]
      (= t 0xcc) [(ubyte d p) (inc p)]
      (= t 0xcd) [(u16 d p) (+ p 2)]
      (= t 0xce) [(u32 d p) (+ p 4)]
      (= t 0xcf) [(i64 d p) (+ p 8)]
      (= t 0xd0) [(long (aget d (int p))) (inc p)]
      (= t 0xd1) [(unchecked-short (u16 d p)) (+ p 2)]
      (= t 0xd2) [(unchecked-int (u32 d p)) (+ p 4)]
      (= t 0xd3) [(i64 d p) (+ p 8)]
      (= t 0xd9) (let [n (ubyte d p)] [(b/latin1 (slice d (inc p) n)) (+ p 1 n)])
      (= t 0xda) (let [n (u16 d p)] [(b/latin1 (slice d (+ p 2) n)) (+ p 2 n)])
      (= t 0xdb) (let [n (u32 d p)] [(b/latin1 (slice d (+ p 4) n)) (+ p 4 n)])
      (= t 0xdc) (read-n d (+ p 2) (u16 d p) read-val)
      (= t 0xdd) (read-n d (+ p 4) (u32 d p) read-val)
      (= t 0xde) (read-map d (+ p 2) (u16 d p))
      (= t 0xdf) (read-map d (+ p 4) (u32 d p))
      (>= t 0xe0) [(- t 0x100) p]
      :else (throw (ex-info "unsupported msgpack type" {:tag (format "0x%02x" t) :offset off})))))

(defn unpack [^bytes data]
  (first (read-val data 0)))

(defn unpack-stream [^bytes data]
  (loop [p 0 acc []]
    (if (>= p (alength data))
      acc
      (let [[v p'] (read-val data p)]
        (recur (long p') (conj acc v))))))
