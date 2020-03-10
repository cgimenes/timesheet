(ns timesheet.core-test
  (:require [clojure.test :refer :all]
            [timesheet.core :refer :all]
            [java-time :as t]))

(def now (truncate (t/local-time)))

(deftest calc-worked-test
  (testing "Normal workday"
    (is (= 
         (calc-worked [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18)]) 
         workday)))
  
  (testing "Today after lunch"
    (is (=
         (calc-worked [(t/local-time 9) (t/local-time 12) (t/local-time 13)])
         (calc-worked [(t/local-time 9) (t/local-time 12) (t/local-time 13) now])))))

(deftest calc-left-test
  (testing "Extra work"
    (is (=
         (calc-left [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 19)])
         zero)))
  
  (testing "Normal workday"
    (is (=
         (calc-left [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18)])
         zero)))

  (testing "Left 1 hour"
    (is (=
         (calc-left [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 17)])
         (t/duration 1 :hours))))

  (testing "Left 30 minutes"
    (is (=
         (calc-left [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 17 30)])
         (t/duration 30 :minutes))))

  (testing "Left 1 hour and 30 minutes"
    (is (=
         (calc-left [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 16 30)])
         (t/duration 90 :minutes)))))

(deftest calc-Balance-test
  (testing "Extra work"
    (is (=
         (calc-balance [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 19)])
         (t/duration 1 :hours))))

  (testing "Normal workday"
    (is (=
         (calc-balance [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18)])
         zero)))

  (testing "Left 1 hour"
    (is (=
         (calc-balance [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 17)])
         (t/negate (t/duration 1 :hours)))))

  (testing "Left 30 minutes"
    (is (=
         (calc-balance [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 17 30)])
         (t/negate (t/duration 30 :minutes)))))

  (testing "Left 1 hour and 30 minutes"
    (is (=
         (calc-balance [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 16 30)])
         (t/negate (t/duration 90 :minutes))))))


(deftest calc-month-balance-test
  (testing "Positive month"
    (is (=
         (calc-month-balance [
                              {:day 1 :punches [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18 20)]}
                              {:day 2 :punches [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18 20)]}
                              {:day 3 :punches [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18 20)]}
                              {:day 4 :punches [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18 20)]}
                              {:day 5 :punches [(t/local-time 9) (t/local-time 12) (t/local-time 13) (t/local-time 18 20)]}])
         (t/duration 100 :minutes)))))