(ns timesheet.core
  (import java.time.YearMonth)
  (:require [java-time :as time]
            [timesheet.filters :refer [month-format]]))

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
  (let [diff (time/minus a b)]
    (if (time/negative? diff)
      zero
      diff)))

(def workday
  "normal workday duration"
  (time/duration 8 :hours))

(defn not-calculated
  "TODO"
  [date]
  (or (time/weekend? date) (time/after? date today)))

(defn day-transformer
  "TODO doc and refactory"
  [{date :date punches :punches allowance :allowance}]
  (let [not-calculated (not-calculated date)
        should-review (should-review date punches)
        sorted-punches (sort punches)]
    (if should-review
      (reduce (fn [{worked  :worked
                    left    :left
                    balance :balance} [a b]]
                (let [diff (time/duration (truncate a) (truncate b))]
                  {:date           date
                   :allowance      allowance
                   :punches        sorted-punches
                   :worked         (time/plus worked diff)
                   :left           (positive-minus left diff)
                   :balance        (time/plus balance diff)
                   :review         true
                   :not-calculated not-calculated}))
              {:date           date
               :allowance      allowance
               :punches        sorted-punches
               :worked         zero
               :left           (if not-calculated zero workday)
               :balance        (if not-calculated zero (if allowance (time/plus (time/negate workday) (first allowance)) (time/negate workday)))
               :review         true
               :not-calculated not-calculated}
              (partition 2 sorted-punches))
      (reduce (fn [{worked  :worked
                    left    :left
                    balance :balance} [a b]]
                (let [diff (time/duration (truncate a) (truncate b))]
                  {:date           date
                   :allowance      allowance
                   :punches        sorted-punches
                   :worked         (time/plus worked diff)
                   :left           (positive-minus left diff)
                   :balance        (time/plus balance diff)
                   :review         false
                   :not-calculated not-calculated}))
              {:date           date
               :allowance      allowance
               :punches        sorted-punches
               :worked         zero
               :left           (if not-calculated zero workday)
               :balance        (if not-calculated zero (if allowance (time/plus (time/negate workday) (first allowance)) (time/negate workday)))
               :review         false
               :not-calculated not-calculated}
              (partition 2 2 [now] sorted-punches)))))

(defn map-balance-corr-transducer
  "reduce (time/plus) the transformations (xform) of coll (key map) sorting by skey"
  [map key xform skey]
  (reduce (fn [acc x] (let [xf (xform x)] (-> acc
                                              (assoc :balance (time/plus (:balance acc) (:balance xf)))
                                              (assoc key (conj (key acc) xf)))))
          (-> map
              (assoc key [])
              (assoc :balance (:correction map)))
          (sort-by skey (key map))))

(defn month-transducer
  [month]
  (map-balance-corr-transducer month
                               :days
                               day-transformer
                               :date))

(defn timesheet-transducer
  [timesheet]
  (map-balance-corr-transducer timesheet
                               :months
                               month-transducer
                               :month))

(defn find-first
  [f coll]
  (some #(if (f %) % nil) coll))

(defn find-month
  "TODO"
  [months date]
  (find-first #(= (month-format date) (:month %)) months))

(defn find-day
  "TODO"
  [{month :month days :days} date]
  (when (= month (month-format date))
    (find-first #(= date (:date %)) days)))

(defn possible-departure
  "TODO"
  [{months :months} date]
  (when-let [day (some-> months
                         (find-month date)
                         (find-day date))]
    (if (= (:left day) zero)
      now
      (when-let [last-punch (last (:punches day))]
        (time/plus last-punch (:left day))))))

(defn complete-month-days
  "TODO"
  [date]
  (let [first-day-of-month (time/adjust date :first-day-of-month)
        month-days (take
                    (.lengthOfMonth (YearMonth/of (time/as date :year) (time/as date :month-of-year)))
                    (time/iterate time/plus first-day-of-month (time/days 1)))]
    (vec (map (fn [x] {:date x :punches [] :allowance nil}) month-days))))

(defn add-month-if-not-present
  "TODO"
  [months date]
  (if-not (find-month months date)
    (conj months {:month (month-format date) :days (complete-month-days date) :correction zero})
    months))

(defn find-first-index
  [f coll]
  (first (keep-indexed #(when (f %2) %1) coll)))

(defn find-month-index
  [months date]
  (find-first-index #(= (month-format date) (:month %)) months))

(defn find-day-index
  [{month :month days :days} date]
  (when (= month (month-format date))
    (find-first-index #(= date (:date %)) days)))

(defn add-punch
  "TODO"
  [{months :months :as timesheet} date time]
  (let [new-months (add-month-if-not-present months date)
        new-timesheet (assoc timesheet :months new-months)
        month-index (find-month-index new-months date)
        day-index (find-day-index (get new-months month-index) date)
        new-punches (conj (get-in new-timesheet [:months month-index :days day-index :punches]) time)]
    (assoc-in new-timesheet [:months month-index :days day-index :punches] new-punches)))

(defn remove-punch
  "TODO"
  [{months :months :as timesheet} date time]
  (let [month-index (find-month-index months date)
        day-index (find-day-index (get months month-index) date)
        new-punches (remove (partial = time) (get-in timesheet [:months month-index :days day-index :punches]))]
    (if month-index
      (assoc-in timesheet [:months month-index :days day-index :punches] new-punches)
      timesheet)))
