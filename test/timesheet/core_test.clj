(ns timesheet.core-test
  (:require [clojure.test :refer :all]
            [timesheet.core :as core]
            [java-time :as time]))

(deftest timesheet-transducer-test
  (testing "Positive timesheet"
    (is (= (:balance (core/timesheet-transducer {:months [{:month "2020-02"
                                                           :days [{:date (time/local-date "2020-02-03")
                                                                   :punches [(time/local-time "09:00")
                                                                             (time/local-time "12:00")
                                                                             (time/local-time "13:00")
                                                                             (time/local-time "18:30")]
                                                                   :allowance [(time/duration -2 :hours) "Médico"]}
                                                                  {:date (time/local-date "2020-02-04")
                                                                   :punches [(time/local-time "09:00")
                                                                             (time/local-time "12:00")
                                                                             (time/local-time "13:00")
                                                                             (time/local-time "18:30")]
                                                                   :allowance nil}]
                                                           :correction (time/duration 5 :minutes)}]
                                                 :correction (time/duration 1 :hours)}))
           (time/duration 5 :minutes))))
  (testing "Empty timesheet"
    (is (= (:balance (core/timesheet-transducer {:months []
                                                 :correction (time/duration)}))
           (time/duration)))))

(deftest possible-departure-test
  (testing "Three punches"
    (is (= (let [timesheet (core/timesheet-transducer {:months [{:month "2020-02"
                                                                 :days [{:date (time/local-date "2020-02-04")
                                                                         :punches [(time/local-time "09:00")
                                                                                   (time/local-time "12:00")
                                                                                   (time/local-time "13:00")]
                                                                         :allowance nil}]
                                                                 :correction (time/duration 5 :minutes)}]
                                                       :correction (time/duration 1 :hours)})]
             (core/possible-departure timesheet (time/local-date "2020-02-04")))
           (time/local-time "18:00"))))
  (testing "No punches"
    (is (= (let [timesheet (core/timesheet-transducer {:months [{:month "2020-02"
                                                                 :days [{:date (time/local-date "2020-02-04")
                                                                         :punches []
                                                                         :allowance nil}]
                                                                 :correction (time/duration 5 :minutes)}]
                                                       :correction (time/duration 1 :hours)})]
             (core/possible-departure timesheet (time/local-date "2020-02-04")))
           nil))))

(deftest add-punch-test
  (testing "Add punch without month"
    (is (= (core/add-punch {:months [] :correction (time/duration)}
                           (time/local-date "2020-02-03")
                           (time/local-time "09:00"))
           {:months [{:month "2020-02"
                      :days [{:date "2020-02-01", :punches [], :allowance nil}
                             {:date "2020-02-02", :punches [], :allowance nil}
                             {:date "2020-02-03", :punches ["09:00"], :allowance nil}
                             {:date "2020-02-04", :punches [], :allowance nil}
                             {:date "2020-02-05", :punches [], :allowance nil}
                             {:date "2020-02-06", :punches [], :allowance nil}
                             {:date "2020-02-07", :punches [], :allowance nil}
                             {:date "2020-02-08", :punches [], :allowance nil}
                             {:date "2020-02-09", :punches [], :allowance nil}
                             {:date "2020-02-10", :punches [], :allowance nil}
                             {:date "2020-02-11", :punches [], :allowance nil}
                             {:date "2020-02-12", :punches [], :allowance nil}
                             {:date "2020-02-13", :punches [], :allowance nil}
                             {:date "2020-02-14", :punches [], :allowance nil}
                             {:date "2020-02-15", :punches [], :allowance nil}
                             {:date "2020-02-16", :punches [], :allowance nil}
                             {:date "2020-02-17", :punches [], :allowance nil}
                             {:date "2020-02-18", :punches [], :allowance nil}
                             {:date "2020-02-19", :punches [], :allowance nil}
                             {:date "2020-02-20", :punches [], :allowance nil}
                             {:date "2020-02-21", :punches [], :allowance nil}
                             {:date "2020-02-22", :punches [], :allowance nil}
                             {:date "2020-02-23", :punches [], :allowance nil}
                             {:date "2020-02-24", :punches [], :allowance nil}
                             {:date "2020-02-25", :punches [], :allowance nil}
                             {:date "2020-02-26", :punches [], :allowance nil}
                             {:date "2020-02-27", :punches [], :allowance nil}
                             {:date "2020-02-28", :punches [], :allowance nil}
                             {:date "2020-02-29", :punches [], :allowance nil}]
                      :correction core/zero}]
            :correction core/zero})))
  (testing "Add punch with month"
    (is (= (core/add-punch {:months [{:month "2020-02"
                                      :days [{:date "2020-02-01", :punches [], :allowance nil}
                                             {:date "2020-02-02", :punches [], :allowance nil}
                                             {:date "2020-02-03", :punches ["09:00"], :allowance nil}
                                             {:date "2020-02-04", :punches [], :allowance nil}
                                             {:date "2020-02-05", :punches [], :allowance nil}
                                             {:date "2020-02-06", :punches [], :allowance nil}
                                             {:date "2020-02-07", :punches [], :allowance nil}
                                             {:date "2020-02-08", :punches [], :allowance nil}
                                             {:date "2020-02-09", :punches [], :allowance nil}
                                             {:date "2020-02-10", :punches [], :allowance nil}
                                             {:date "2020-02-11", :punches [], :allowance nil}
                                             {:date "2020-02-12", :punches [], :allowance nil}
                                             {:date "2020-02-13", :punches [], :allowance nil}
                                             {:date "2020-02-14", :punches [], :allowance nil}
                                             {:date "2020-02-15", :punches [], :allowance nil}
                                             {:date "2020-02-16", :punches [], :allowance nil}
                                             {:date "2020-02-17", :punches [], :allowance nil}
                                             {:date "2020-02-18", :punches [], :allowance nil}
                                             {:date "2020-02-19", :punches [], :allowance nil}
                                             {:date "2020-02-20", :punches [], :allowance nil}
                                             {:date "2020-02-21", :punches [], :allowance nil}
                                             {:date "2020-02-22", :punches [], :allowance nil}
                                             {:date "2020-02-23", :punches [], :allowance nil}
                                             {:date "2020-02-24", :punches [], :allowance nil}
                                             {:date "2020-02-25", :punches [], :allowance nil}
                                             {:date "2020-02-26", :punches [], :allowance nil}
                                             {:date "2020-02-27", :punches [], :allowance nil}
                                             {:date "2020-02-28", :punches [], :allowance nil}
                                             {:date "2020-02-29", :punches [], :allowance nil}]
                                      :correction core/zero}]
                            :correction core/zero}
                           (time/local-date "2020-02-03")
                           (time/local-time "12:00"))
           {:months [{:month "2020-02"
                      :days [{:date "2020-02-01", :punches [], :allowance nil}
                             {:date "2020-02-02", :punches [], :allowance nil}
                             {:date "2020-02-03", :punches ["09:00" "12:00"], :allowance nil}
                             {:date "2020-02-04", :punches [], :allowance nil}
                             {:date "2020-02-05", :punches [], :allowance nil}
                             {:date "2020-02-06", :punches [], :allowance nil}
                             {:date "2020-02-07", :punches [], :allowance nil}
                             {:date "2020-02-08", :punches [], :allowance nil}
                             {:date "2020-02-09", :punches [], :allowance nil}
                             {:date "2020-02-10", :punches [], :allowance nil}
                             {:date "2020-02-11", :punches [], :allowance nil}
                             {:date "2020-02-12", :punches [], :allowance nil}
                             {:date "2020-02-13", :punches [], :allowance nil}
                             {:date "2020-02-14", :punches [], :allowance nil}
                             {:date "2020-02-15", :punches [], :allowance nil}
                             {:date "2020-02-16", :punches [], :allowance nil}
                             {:date "2020-02-17", :punches [], :allowance nil}
                             {:date "2020-02-18", :punches [], :allowance nil}
                             {:date "2020-02-19", :punches [], :allowance nil}
                             {:date "2020-02-20", :punches [], :allowance nil}
                             {:date "2020-02-21", :punches [], :allowance nil}
                             {:date "2020-02-22", :punches [], :allowance nil}
                             {:date "2020-02-23", :punches [], :allowance nil}
                             {:date "2020-02-24", :punches [], :allowance nil}
                             {:date "2020-02-25", :punches [], :allowance nil}
                             {:date "2020-02-26", :punches [], :allowance nil}
                             {:date "2020-02-27", :punches [], :allowance nil}
                             {:date "2020-02-28", :punches [], :allowance nil}
                             {:date "2020-02-29", :punches [], :allowance nil}]
                      :correction core/zero}]
            :correction core/zero}))))

(deftest remove-punch-test
  (testing "Remove punch without month"
    (is (= (core/remove-punch {:months [] :correction (time/duration)}
                              (time/local-date "2020-02-03")
                              (time/local-time "09:00"))
           {:months [] :correction (time/duration)})))
  (testing "Remove punch with month"
    (is (= (core/remove-punch {:months [{:month "2020-02"
                                         :days [{:date "2020-02-01", :punches [], :allowance nil}
                                                {:date "2020-02-02", :punches [], :allowance nil}
                                                {:date "2020-02-03", :punches ["09:00"], :allowance nil}
                                                {:date "2020-02-04", :punches [], :allowance nil}
                                                {:date "2020-02-05", :punches [], :allowance nil}
                                                {:date "2020-02-06", :punches [], :allowance nil}
                                                {:date "2020-02-07", :punches [], :allowance nil}
                                                {:date "2020-02-08", :punches [], :allowance nil}
                                                {:date "2020-02-09", :punches [], :allowance nil}
                                                {:date "2020-02-10", :punches [], :allowance nil}
                                                {:date "2020-02-11", :punches [], :allowance nil}
                                                {:date "2020-02-12", :punches [], :allowance nil}
                                                {:date "2020-02-13", :punches [], :allowance nil}
                                                {:date "2020-02-14", :punches [], :allowance nil}
                                                {:date "2020-02-15", :punches [], :allowance nil}
                                                {:date "2020-02-16", :punches [], :allowance nil}
                                                {:date "2020-02-17", :punches [], :allowance nil}
                                                {:date "2020-02-18", :punches [], :allowance nil}
                                                {:date "2020-02-19", :punches [], :allowance nil}
                                                {:date "2020-02-20", :punches [], :allowance nil}
                                                {:date "2020-02-21", :punches [], :allowance nil}
                                                {:date "2020-02-22", :punches [], :allowance nil}
                                                {:date "2020-02-23", :punches [], :allowance nil}
                                                {:date "2020-02-24", :punches [], :allowance nil}
                                                {:date "2020-02-25", :punches [], :allowance nil}
                                                {:date "2020-02-26", :punches [], :allowance nil}
                                                {:date "2020-02-27", :punches [], :allowance nil}
                                                {:date "2020-02-28", :punches [], :allowance nil}
                                                {:date "2020-02-29", :punches [], :allowance nil}]
                                         :correction core/zero}]
                               :correction core/zero}
                              (time/local-date "2020-02-03")
                              (time/local-time "09:00"))
           {:months [{:month "2020-02"
                      :days [{:date "2020-02-01", :punches [], :allowance nil}
                             {:date "2020-02-02", :punches [], :allowance nil}
                             {:date "2020-02-03", :punches [], :allowance nil}
                             {:date "2020-02-04", :punches [], :allowance nil}
                             {:date "2020-02-05", :punches [], :allowance nil}
                             {:date "2020-02-06", :punches [], :allowance nil}
                             {:date "2020-02-07", :punches [], :allowance nil}
                             {:date "2020-02-08", :punches [], :allowance nil}
                             {:date "2020-02-09", :punches [], :allowance nil}
                             {:date "2020-02-10", :punches [], :allowance nil}
                             {:date "2020-02-11", :punches [], :allowance nil}
                             {:date "2020-02-12", :punches [], :allowance nil}
                             {:date "2020-02-13", :punches [], :allowance nil}
                             {:date "2020-02-14", :punches [], :allowance nil}
                             {:date "2020-02-15", :punches [], :allowance nil}
                             {:date "2020-02-16", :punches [], :allowance nil}
                             {:date "2020-02-17", :punches [], :allowance nil}
                             {:date "2020-02-18", :punches [], :allowance nil}
                             {:date "2020-02-19", :punches [], :allowance nil}
                             {:date "2020-02-20", :punches [], :allowance nil}
                             {:date "2020-02-21", :punches [], :allowance nil}
                             {:date "2020-02-22", :punches [], :allowance nil}
                             {:date "2020-02-23", :punches [], :allowance nil}
                             {:date "2020-02-24", :punches [], :allowance nil}
                             {:date "2020-02-25", :punches [], :allowance nil}
                             {:date "2020-02-26", :punches [], :allowance nil}
                             {:date "2020-02-27", :punches [], :allowance nil}
                             {:date "2020-02-28", :punches [], :allowance nil}
                             {:date "2020-02-29", :punches [], :allowance nil}]
                      :correction core/zero}]
            :correction core/zero})))
  (testing "Remove punch with month but without punch"
    (is (= (core/remove-punch {:months [{:month "2020-02"
                                         :days [{:date "2020-02-01", :punches [], :allowance nil}
                                                {:date "2020-02-02", :punches [], :allowance nil}
                                                {:date "2020-02-03", :punches ["09:00"], :allowance nil}
                                                {:date "2020-02-04", :punches [], :allowance nil}
                                                {:date "2020-02-05", :punches [], :allowance nil}
                                                {:date "2020-02-06", :punches [], :allowance nil}
                                                {:date "2020-02-07", :punches [], :allowance nil}
                                                {:date "2020-02-08", :punches [], :allowance nil}
                                                {:date "2020-02-09", :punches [], :allowance nil}
                                                {:date "2020-02-10", :punches [], :allowance nil}
                                                {:date "2020-02-11", :punches [], :allowance nil}
                                                {:date "2020-02-12", :punches [], :allowance nil}
                                                {:date "2020-02-13", :punches [], :allowance nil}
                                                {:date "2020-02-14", :punches [], :allowance nil}
                                                {:date "2020-02-15", :punches [], :allowance nil}
                                                {:date "2020-02-16", :punches [], :allowance nil}
                                                {:date "2020-02-17", :punches [], :allowance nil}
                                                {:date "2020-02-18", :punches [], :allowance nil}
                                                {:date "2020-02-19", :punches [], :allowance nil}
                                                {:date "2020-02-20", :punches [], :allowance nil}
                                                {:date "2020-02-21", :punches [], :allowance nil}
                                                {:date "2020-02-22", :punches [], :allowance nil}
                                                {:date "2020-02-23", :punches [], :allowance nil}
                                                {:date "2020-02-24", :punches [], :allowance nil}
                                                {:date "2020-02-25", :punches [], :allowance nil}
                                                {:date "2020-02-26", :punches [], :allowance nil}
                                                {:date "2020-02-27", :punches [], :allowance nil}
                                                {:date "2020-02-28", :punches [], :allowance nil}
                                                {:date "2020-02-29", :punches [], :allowance nil}]
                                         :correction core/zero}]
                               :correction core/zero}
                              (time/local-date "2020-02-03")
                              (time/local-time "12:00"))
           {:months [{:month "2020-02"
                      :days [{:date "2020-02-01", :punches [], :allowance nil}
                             {:date "2020-02-02", :punches [], :allowance nil}
                             {:date "2020-02-03", :punches ["09:00"], :allowance nil}
                             {:date "2020-02-04", :punches [], :allowance nil}
                             {:date "2020-02-05", :punches [], :allowance nil}
                             {:date "2020-02-06", :punches [], :allowance nil}
                             {:date "2020-02-07", :punches [], :allowance nil}
                             {:date "2020-02-08", :punches [], :allowance nil}
                             {:date "2020-02-09", :punches [], :allowance nil}
                             {:date "2020-02-10", :punches [], :allowance nil}
                             {:date "2020-02-11", :punches [], :allowance nil}
                             {:date "2020-02-12", :punches [], :allowance nil}
                             {:date "2020-02-13", :punches [], :allowance nil}
                             {:date "2020-02-14", :punches [], :allowance nil}
                             {:date "2020-02-15", :punches [], :allowance nil}
                             {:date "2020-02-16", :punches [], :allowance nil}
                             {:date "2020-02-17", :punches [], :allowance nil}
                             {:date "2020-02-18", :punches [], :allowance nil}
                             {:date "2020-02-19", :punches [], :allowance nil}
                             {:date "2020-02-20", :punches [], :allowance nil}
                             {:date "2020-02-21", :punches [], :allowance nil}
                             {:date "2020-02-22", :punches [], :allowance nil}
                             {:date "2020-02-23", :punches [], :allowance nil}
                             {:date "2020-02-24", :punches [], :allowance nil}
                             {:date "2020-02-25", :punches [], :allowance nil}
                             {:date "2020-02-26", :punches [], :allowance nil}
                             {:date "2020-02-27", :punches [], :allowance nil}
                             {:date "2020-02-28", :punches [], :allowance nil}
                             {:date "2020-02-29", :punches [], :allowance nil}]
                      :correction core/zero}]
            :correction core/zero}))))