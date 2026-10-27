(ns borgclj.mft-test
  (:require [clojure.test :refer [deftest is testing]]
            [borgclj.win.mft :as mft]))

(deftest throttle-keeps-the-scan-from-drowning-in-reports
  (testing "a burst of calls yields one report, and nil means no reporting at all"
    (let [seen (atom [])
          report (mft/throttled #(swap! seen conj %))]
      (dotimes [i 10000] (report {:n i}))
      (is (= 1 (count @seen)) "one second has not passed, so only the first call reports")
      (is (= {:n 0} (first @seen)) "and it carries the first sample, not the last"))
    (let [report (mft/throttled nil)]
      (is (nil? (report {:n 1})) "without a reporter the scan must not pay for one"))))
