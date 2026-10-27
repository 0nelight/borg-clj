(ns borgclj.key
  (:require [borgclj.bytes :as b]
            [borgclj.compress :as compress])
  (:import [java.util Arrays]))

(def ^:const type-plaintext 0x02)

(defn plaintext-key
  ([] (plaintext-key nil))
  ([compression] {:type type-plaintext :chunk-seed 0
                  :compression (compress/parse-spec compression)}))

(defn id-hash ^bytes [k ^bytes data]
  (case (long (:type k))
    0x02 (b/sha256 data)
    (throw (ex-info "unsupported key type" {:type (:type k)}))))

(defn decrypt ^bytes [k id0 ^bytes envelope]
  (let [id (when id0 (b/->bytes id0))]
  (let [t (bit-and (aget envelope 0) 0xff)]
    (when-not (= t (:type k))
      (throw (ex-info "invalid encryption envelope" {:found t :want (:type k)})))
    (let [data (compress/decompress (Arrays/copyOfRange envelope 1 (alength envelope)))]
      (when id
        (when-not (Arrays/equals ^bytes (id-hash k data) id)
          (throw (ex-info "chunk id mismatch" {:id (b/hex id)}))))
      data))))

(defn encrypt ^bytes [k ^bytes data]
  (let [payload (compress/compress (:compression k {:kind :none}) data)
        out (byte-array (inc (alength payload)))]
    (aset out 0 (unchecked-byte (:type k)))
    (System/arraycopy payload 0 out 1 (alength payload))
    out))

(defn tam-key ^bytes [k ^bytes salt ^String context]
  (case (long (:type k))
    0x02 (let [c (b/unlatin1 context)
               out (byte-array (+ (alength salt) (alength c)))]
           (System/arraycopy salt 0 out 0 (alength salt))
           (System/arraycopy c 0 out (alength salt) (alength c))
           out)
    (throw (ex-info "unsupported key type" {:type (:type k)}))))

(defn hmac-sha512 ^bytes [^bytes k ^bytes data]
  (let [m (javax.crypto.Mac/getInstance "HmacSHA512")]
    (.init m (javax.crypto.spec.SecretKeySpec. k "HmacSHA512"))
    (.doFinal m data)))
