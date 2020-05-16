(ns timesheet.core
  (:require [java-time :as time]))

(def today
  "today's date"
  (time/local-date))

(def zero
  "a zeroed duration"
  (time/duration))

(def now
  "now's time"
  (time/local-time))

(def truncate
  "truncate duration to minutes"
  #(time/truncate-to % :minutes))

(defn should-review
  "returns true if it's an incomplete day (with odd) and date is before today"
  [date punches]
  (and ((comp not even? count) punches)
       (time/before? date today)))

(defn positive-minus
  "returns the difference between two durations only if the difference is greater than zero, 
   otherwise returns a zeroed duration"
  [a b]
  (let [diff (time/minus a b)] (if (time/negative? diff)
                                 zero
                                 diff)))

(def workday
  "normal workday duration"
  (time/duration 8 :hours))

(defn day-reducer
  "TODO"
  [day]
  (let [date (time/local-date (:date day))
        punches (:punches day)
        allowance (:allowance day)
        not-calculated (or (time/weekend? date) (time/after? date today))]
    (if (should-review date punches)
      {:date           (:date day)
       :punches        punches
       :worked         zero
       :left           zero
       :balance        zero
       :review         true
       :not-calculated not-calculated}
      (reduce (fn [{worked  :worked
                    left    :left
                    balance :balance} [a b]]
                (let [diff (time/duration (truncate a) (truncate b))]
                  {:date           (:date day)
                   :punches        punches
                   :worked         (time/plus worked diff)
                   :left           (positive-minus left diff)
                   :balance        (time/plus balance diff)
                   :review         false
                   :not-calculated not-calculated}))
              {:date           (:date day)
               :punches        punches
               :worked         zero
               :left           (if not-calculated zero workday)
               :balance        (if not-calculated zero (if allowance (time/plus (time/negate workday) (first allowance)) (time/negate workday)))
               :review         false
               :not-calculated not-calculated}
              (->> punches
                   (map time/local-time)
                   (partition 2 2 [now]))))))

(defn map-balance-corr-transducer
  "TODO"
  [map key xform]
  (reduce (fn [acc x] (let [xf (xform x)] (-> acc
                                              (assoc :balance (time/plus (:balance acc) (:balance xf)))
                                              (assoc key (conj (key acc) xf)))))
          (-> map
              (assoc key [])
              (assoc :balance (:correction map)))
          (key map)))

(defn month-transducer
  "TODO"
  [month]
  (map-balance-corr-transducer month
                               :days
                               day-reducer))

(defn timesheet-transducer
  "TODO"
  [timesheet]
  (map-balance-corr-transducer timesheet
                               :months
                               month-transducer))

(timesheet-transducer {:months [{:month "2020-02"
                                 :days [{:date "2020-02-01" :punches [] :allowance nil}
                                        {:date "2020-02-02" :punches [] :allowance nil}
                                        {:date "2020-02-03" :punches ["09:28" "11:48" "13:45" "16:30"] :allowance [(time/duration 1 :hours) "Médico"]}
                                        {:date "2020-02-04" :punches ["10:50" "13:14" "14:03" "14:42" "14:44" "21:01"] :allowance nil}
                                        {:date "2020-02-05" :punches [] :allowance nil}
                                        {:date "2020-02-06" :punches [] :allowance nil}
                                        {:date "2020-02-07" :punches [] :allowance nil}
                                        {:date "2020-02-08" :punches [] :allowance nil}
                                        {:date "2020-02-09" :punches [] :allowance nil}
                                        {:date "2020-02-10" :punches [] :allowance nil}
                                        {:date "2020-02-11" :punches [] :allowance nil}
                                        {:date "2020-02-12" :punches [] :allowance nil}
                                        {:date "2020-02-13" :punches [] :allowance nil}
                                        {:date "2020-02-14" :punches [] :allowance nil}
                                        {:date "2020-02-15" :punches [] :allowance nil}
                                        {:date "2020-02-16" :punches [] :allowance nil}
                                        {:date "2020-02-17" :punches [] :allowance nil}
                                        {:date "2020-02-18" :punches [] :allowance nil}
                                        {:date "2020-02-19" :punches [] :allowance nil}
                                        {:date "2020-02-20" :punches [] :allowance nil}
                                        {:date "2020-02-21" :punches [] :allowance nil}
                                        {:date "2020-02-22" :punches [] :allowance nil}
                                        {:date "2020-02-23" :punches [] :allowance nil}
                                        {:date "2020-02-24" :punches [] :allowance nil}
                                        {:date "2020-02-25" :punches [] :allowance nil}
                                        {:date "2020-02-26" :punches [] :allowance nil}
                                        {:date "2020-02-27" :punches [] :allowance nil}
                                        {:date "2020-02-28" :punches [] :allowance nil}
                                        {:date "2020-02-29" :punches [] :allowance nil}]
                                 :correction (time/duration 5 :minutes)}]
                       :correction (time/duration 12 :hours)})

(get-in (timesheet-transducer {:months [{:month "2020-02"
                                         :days [{:date "2020-02-03" :punches ["09:28" "11:48" "13:45" "16:30"] :allowance [(time/duration 2 :hours) "Médico"]}]
                                         :correction (time/duration 5 :minutes)}]
                               :correction (time/duration 12 :hours)}) [:months 0 :days 0])