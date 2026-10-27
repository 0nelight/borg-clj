(ns borgclj.create-names-test
  (:require [borgclj.create :as create]
            [clojure.test :refer [deftest is testing]]))

(deftest an-archive-name-is-taken-only-once
  (let [old {:archives {"tower-2026-09-20" {:id (byte-array 32) :time "2026-09-20T10:00:00"}}}]
    (testing "a new name passes"
      (is (nil? (create/refuse-taken! old "tower-2026-09-27"))))
    (testing "the same name is refused instead of silently replacing the older archive"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already exists"
                            (create/refuse-taken! old "tower-2026-09-20"))))
    (testing "an empty repository takes any name"
      (is (nil? (create/refuse-taken! {:archives {}} "tower-2026-09-20"))))))
