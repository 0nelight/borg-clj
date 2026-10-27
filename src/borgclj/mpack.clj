(ns borgclj.mpack
  (:require [borgclj.bytes :as b])
  (:import [java.io ByteArrayOutputStream DataOutputStream]))

(declare write-val!)

(defn- w-len! [^DataOutputStream o n fix fix-max b8 b16 b32]
  (cond
    (and fix (< (long n) (long fix-max))) (.writeByte o (unchecked-int (bit-or (long fix) (long n))))
    (and b8 (< (long n) 256)) (do (.writeByte o (unchecked-int (long b8))) (.writeByte o (unchecked-int (long n))))
    (< (long n) 65536) (do (.writeByte o (unchecked-int (long b16))) (.writeShort o (unchecked-int (long n))))
    :else (do (.writeByte o (unchecked-int (long b32))) (.writeInt o (unchecked-int (long n))))))

(defn- write-int! [^DataOutputStream o ^long n]
  (cond
    (and (<= 0 n) (< n 128)) (.writeByte o (unchecked-int n))
    (and (<= -32 n) (neg? n)) (.writeByte o (unchecked-int (bit-and n 0xff)))
    (and (<= 0 n) (<= n 0xffffffff)) (do (.writeByte o 0xce) (.writeInt o (unchecked-int (long n))))
    :else (do (.writeByte o 0xd3) (.writeLong o n))))

(defn- map-key [k]
  (cond
    (keyword? k) (name k)
    (string? k) k
    (bytes? k) k
    :else (str k)))

(defn- write-map! [^DataOutputStream o m]
  (w-len! o (count m) 0x80 16 nil 0xde 0xdf)
  (doseq [k (sort-by #(if (bytes? %) (b/latin1 %) (str %)) (map map-key (keys m)))]
    (write-val! o k)
    (write-val! o (get m (if (string? k) (keyword k) k) (get m k)))))

(defn write-val! [^DataOutputStream o v]
  (cond
    (nil? v) (.writeByte o 0xc0)
    (true? v) (.writeByte o 0xc3)
    (false? v) (.writeByte o 0xc2)
    (integer? v) (write-int! o (long v))
    (bytes? v) (let [^bytes a v] (w-len! o (alength a) 0xa0 32 0xd9 0xda 0xdb) (.write o a))
    (string? v) (let [^bytes a (b/unlatin1 v)] (w-len! o (alength a) 0xa0 32 0xd9 0xda 0xdb) (.write o a))
    (map? v) (write-map! o v)
    (or (vector? v) (seq? v) (set? v)) (let [c (vec v)]
                                         (w-len! o (count c) 0x90 16 nil 0xdc 0xdd)
                                         (doseq [x c] (write-val! o x)))
    (double? v) (do (.writeByte o 0xcb) (.writeDouble o (double v)))
    :else (throw (ex-info "cannot pack" {:type (type v)}))))

(defn pack ^bytes [v]
  (let [bos (ByteArrayOutputStream.)
        o (DataOutputStream. bos)]
    (write-val! o v)
    (.flush o)
    (.toByteArray bos)))
