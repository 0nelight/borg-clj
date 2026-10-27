(ns borgclj.gui-i18n-test
  (:require [borgclj.gui.i18n :as i18n]
            [clojure.test :refer [deftest is]]))

(defn- specifiers [^String s]
  (re-seq #"%[-#+ 0,(]*\d*(?:\.\d+)?[a-zA-Z%]" s))

(deftest every-text-exists-in-both-languages
  (doseq [[k e] i18n/strings]
    (is (string? (:en e)) (str k " has no English text"))
    (is (string? (:de e)) (str k " has no German text"))
    (is (= (specifiers (:en e)) (specifiers (:de e)))
        (str k ": English and German take different format arguments"))))

(deftest texts-follow-the-language
  (let [before @i18n/lang]
    (try
      (reset! i18n/lang :en)
      (is (= "Disk 2" (i18n/t :c/disk-n 2)))
      (reset! i18n/lang :de)
      (is (= "Laufwerk 2" (i18n/t :c/disk-n 2)))
      (is (thrown? clojure.lang.ExceptionInfo (i18n/t :no/such-key)))
      (finally (reset! i18n/lang before)))))
