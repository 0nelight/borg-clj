(ns borgclj.chunker
  (:require [borgclj.buzhash-table :as bt])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file Path StandardOpenOption]))

(def ^:const window 4095)

(def data-params {:min-exp 19 :max-exp 23 :mask-bits 21})
(def items-params {:min-exp 15 :max-exp 19 :mask-bits 17})

(defn table ^ints [^long seed]
  (let [t (int-array 256)]
    (dotimes [i 256]
      (aset t i (unchecked-int (bit-xor (aget ^ints bt/table-base i) (unchecked-int seed)))))
    t))

(defn- u32 ^long [^long v] (bit-and v 0xffffffff))

(defn- rol ^long [^long v ^long s]
  (u32 (bit-or (bit-shift-left v s)
               (unsigned-bit-shift-right v (bit-and (- 32 s) 0x1f)))))

(defn- h32 ^long [^ints h ^bytes b ^long i]
  (bit-and (aget h (bit-and (aget b i) 0xff)) 0xffffffff))

(defn buzhash ^long [^bytes b ^long off ^long len ^ints h]
  (loop [i (dec len) p off sum 0]
    (if (pos? i)
      (recur (dec i) (inc p) (bit-xor sum (rol (h32 h b p) (bit-and i 0x1f))))
      (u32 (bit-xor sum (h32 h b p))))))

(defn- roll ^long [^long sum ^bytes b ^long p ^ints h]
  (u32 (bit-xor (rol sum 1)
                (rol (h32 h b p) (bit-and window 0x1f))
                (h32 h b (+ p window)))))

(defn cut ^long [^bytes buf ^long n ^ints h params]
  (let [min-size (bit-shift-left 1 (long (:min-exp params)))
        m (dec (bit-shift-left 1 (long (:mask-bits params))))
        stop (- n window)]
    (if (< n (+ min-size window 1))
      n
      (loop [p min-size sum (buzhash buf min-size window h)]
        (cond
          (>= p stop) n
          (zero? (bit-and sum m)) p
          :else (recur (inc p) (roll sum buf p h)))))))

(defn chunks
  ([^Path path ^long seed] (chunks path seed data-params))
  ([^Path path ^long seed params]
  (let [h (table seed)
        cap (bit-shift-left 1 (long (:max-exp params)))
        ch (FileChannel/open path (into-array StandardOpenOption [StandardOpenOption/READ]))
        size (.size ch)]
    (letfn [(step [^long pos]
              (lazy-seq
                (if (>= pos size)
                  (do (.close ch) nil)
                  (let [n (min cap (- size pos))
                        bb (ByteBuffer/allocate n)]
                    (.position ch pos)
                    (while (pos? (.remaining bb)) (.read ch bb))
                    (let [buf (.array bb)
                          len (cut buf n h params)]
                      (cons (if (= len n) buf (java.util.Arrays/copyOfRange buf 0 (int len)))
                            (step (+ pos len))))))))]
      (step 0)))))

(defn chunk-stream
  ([^bytes data ^long seed] (chunk-stream data seed data-params))
  ([^bytes data ^long seed params]
   (let [h (table seed)
         cap (bit-shift-left 1 (long (:max-exp params)))
         total (alength data)]
     (loop [pos 0 acc []]
       (if (>= pos total)
         acc
         (let [n (min cap (- total pos))
               buf (java.util.Arrays/copyOfRange data (int pos) (int (+ pos n)))
               len (cut buf n h params)]
           (recur (+ pos len)
                  (conj acc (if (= len n) buf (java.util.Arrays/copyOfRange buf 0 (int len)))))))))))
