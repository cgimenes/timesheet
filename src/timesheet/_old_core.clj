(defn day-data
[date]
(let [review (should-review-day date)])
(-> day
    (update :date time/local-date)
    (assoc :worked (calc-if-not-review calc-worked punches review))
    (assoc :left (calc-if-not-review calc-left punches review))
    (assoc :balance (calc-if-not-review calc-balance punches review))
    (assoc :review review)))

(defn month-data
  "TODO"
  [month days]
  (as-> (vai month days) $
    {:month month :days (map day-data $)}
    (assoc $ :balance (calc-month-balance (:days $)))))

(defn calc-worked
  "TODO"
  [punches]
  (->> punches
       (map time/local-time)
       (partition 2 2 [(time/local-time)])
       (map (fn [[a b]] (time/duration (truncate a) (truncate b))))
       (reduce time/plus)))

(defn calc-balance
  "TODO"
  [punches]
  (let [left (calc-left punches)]
    (if (> (time/as left :minutes) 0)
      (time/negate left)
      (time/minus (calc-worked punches) workday))))

(def calc-month-balance
  "TODO"
  #(reduce time/plus (map :balance %)))

(defn calc-if-not-review
  "TODO"
  [calc punches review]
  (if (not review)
    (calc punches)
    zero))