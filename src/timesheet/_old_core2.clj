(def workday
  "normal workday duration"
  (time/duration 8 :hours))

(def zero 
  "a zeroed duration"
  (time/duration))

(def truncate
  "truncate duration to minutes"
  #(time/truncate-to % :minutes))

(def today
  "today's date"
  (time/local-date))

(def now
  "now's time"
  (time/local-time))

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

(defn reduce-day
  "TODO"
  [date punches]
  (let [not-calculated (or (time/weekend? date) (time/after? date today))]
    (if (should-review date punches)
      {:punches        punches
       :worked         zero
       :left           zero
       :balance        zero
       :review         true
       :not-calculated not-calculated}
      (reduce (fn [{worked  :worked
                    left    :left
                    balance :balance
                    } [a b]]
                (let [diff (time/duration (truncate a) (truncate b))]
                  {:punches        punches
                   :worked         (time/plus worked diff)
                   :left           (positive-minus left diff)
                   :balance        (time/plus balance diff)
                   :review         false
                   :not-calculated not-calculated}))
              {:punches        punches
               :worked         zero
               :left           (if not-calculated zero workday)
               :balance        (if not-calculated zero (time/negate workday))
               :review         false
               :not-calculated not-calculated}
              (->> punches
                   (map time/local-time)
                   (partition 2 2 [now]))))))

(defn assoc-merge
  "TODO"
  [m f k v]
  (if (contains? m k)
    (assoc m k (f (get m k) v))
    (assoc m k v)))

(defn month-merger
  "TODO"
  [& months]
  {:days    (reduce merge (reverse (map :days months)))
   :balance (reduce time/plus (map :balance months))})
  
(defn complete-month-days
  "TODO"
  [month]
  (into {} (let [date       (apply time/local-date (map #(Integer/parseInt %) (str/split month #"-")))
                 month-days (take
                             (.lengthOfMonth (YearMonth/of (time/as date :year) (time/as date :month-of-year)))
                             (time/iterate time/plus date (time/days 1)))]
             (map (fn [x] {(keyword (str x)) {:punches []}}) month-days))))

(defn months-contains-date
  [months date]
  (let [month-number (month-format date)
        day-number   (time/as date :day-of-month)]
    (and
     (contains? months month-number)
     (contains? (:days (get months month-number)) day-number))))

(defn timesheet-reducer
  "TODO"
  [{total-balance :balance
    months        :months
    :as           timesheet} [k {punches :punches}]]
  (let [date (time/local-date (name k))]
    (if (months-contains-date months date)
      timesheet
      (let [{day-balance :balance
             :as         day}     (reduce-day date punches)
            month-number                        (month-format date)
            day-number                          (time/as date :day-of-month)
            {month-balance :balance
             :as           month} {:days    (sorted-map day-number day)
                                   :balance day-balance}]
        {:balance (time/plus total-balance month-balance)
         :months  (assoc-merge months
                               month-merger
                               month-number
                               month)}))))

(defn reduce-timesheet
  "TODO"
  [dates]
  (as-> dates $
    (reduce timesheet-reducer {:balance zero
                               :months  (sorted-map)} $)
    (reduce timesheet-reducer $ (into (sorted-map) (map complete-month-days (keys (:months $)))))))

(defn possible-departure
  "TODO"
  [left]
  (time/plus now left))

; :possible-departure (-> timesheet
;                         :months
;                         (get (month-format today))
;                         :days
;                         (get (time/as today :day-of-month))
;                         :left
;                         possible-departure)
