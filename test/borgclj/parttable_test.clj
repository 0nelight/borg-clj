(ns borgclj.parttable-test
  (:require [borgclj.parttable :as pt]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio ByteBuffer ByteOrder]))

(defn- disk
  "A byte array standing in for a disk, and a read-at over it."
  [^long size]
  (let [b (byte-array size)]
    [b (fn [^long off ^long len] (java.util.Arrays/copyOfRange b (int off) (int (+ off len))))]))

(defn- le ^ByteBuffer [^bytes b] (.order (ByteBuffer/wrap b) ByteOrder/LITTLE_ENDIAN))

(defn- entry! [^bytes b sector-off slot active type lba n]
  (let [o (+ sector-off 0x1BE (* 16 slot))]
    (aset b (int o) (unchecked-byte (if active 0x80 0)))
    (aset b (int (+ o 4)) (unchecked-byte type))
    (.putInt (le b) (int (+ o 8)) (int lba))
    (.putInt (le b) (int (+ o 12)) (int n))))

(defn- sign! [^bytes b ^long sector-off]
  (aset b (int (+ sector-off 0x1FE)) (unchecked-byte 0x55))
  (aset b (int (+ sector-off 0x1FF)) (unchecked-byte 0xAA)))

(deftest mbr-with-a-logical-partition
  (let [[b read-at] (disk (* 4096 512))]
    ;; primary NTFS at 2048, active; extended at 3072 with one logical at +2
    (entry! b 0 0 true 0x07 2048 512)
    (entry! b 0 1 false 0x05 3072 1024)
    (sign! b 0)
    (entry! b (* 3072 512) 0 false 0x83 2 500)
    (sign! b (* 3072 512))
    (let [t (pt/read-table read-at)]
      (is (= :mbr (:style t)))
      (is (= (* 2048 512) (:head t)) "MBR and Track 0 is everything before the first partition")
      (is (= [[1 (* 2048 512) true nil] [5 (* 3074 512) false true]]
             (mapv (juxt :number :offset (comp boolean :active) :logical) (:parts t))))
      (is (= ["NTFS" "Linux"] (mapv pt/type-name (:parts t)))))))

(deftest gpt
  (let [[b read-at] (disk (* 4096 512))
        bb (le b)]
    (entry! b 0 0 false 0xEE 1 4095)
    (sign! b 0)
    (System/arraycopy (.getBytes "EFI PART" "US-ASCII") 0 b 512 8)
    (.putLong bb (+ 512 72) 2)   ; entries start at LBA 2
    (.putInt bb (+ 512 80) 4)    ; four entries
    (.putInt bb (+ 512 84) 128)  ; of 128 bytes
    ;; entry 1: EFI system partition, LBA 2048..2303, name "EFI"
    (let [o 1024
          efi (byte-array [0x28 0x73 0x2A 0xC1 0x1F 0xF8 0xD2 0x11 0xBA 0x4B 0x00 0xA0 0xC9 0x3E 0xC9 0x3B])]
      (System/arraycopy efi 0 b o 16)
      (.putLong bb (+ o 32) 2048)
      (.putLong bb (+ o 40) 2303)
      (System/arraycopy (.getBytes "EFI" "UTF-16LE") 0 b (+ o 56) 6))
    (let [t (pt/read-table read-at)
          [p] (:parts t)]
      (is (= :gpt (:style t)))
      (is (= 1 (count (:parts t))))
      (is (= [1 (* 2048 512) (* 256 512) "EFI"] [(:number p) (:offset p) (:length p) (:label p)]))
      (is (= "EFI System" (pt/type-name p))))))

(deftest a-boot-sector-is-not-a-partition-table
  (testing "NTFS boot sectors end in 55AA too; entries at offset 0 are rejected"
    (let [[b read-at] (disk (* 64 512))]
      (entry! b 0 0 false 0x07 0 64)
      (sign! b 0)
      (is (nil? (pt/read-table read-at))))))
