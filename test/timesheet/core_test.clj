(ns timesheet.core-test
  (:require [clojure.test :refer :all]
            [timesheet.core :refer :all]
            [java-time :as t]))

;; (def now (truncate (t/local-time)))

(deftest calc-month-balance-test
  (testing "Positive month"
    (is (= (+ 1 1) 2))))