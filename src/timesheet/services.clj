(ns timesheet.services
  (:require [java-time :as time]
            [timesheet.filters :refer [time-format date-format]]
            [timesheet.db :as db]
            [timesheet.core :as core]))

(defn user-data
  "TODO"
  [{dates :dates
    email :email}]
  (let [timesheet (core/reduce-timesheet dates)] {:timesheet          timesheet
                                             :months             (keys (:months timesheet))
                                             :possible-departure (core/possible-departure core/zero)
                                             :email              email}))

(defn add-punch
  "TODO"
  [db {dates :dates
       id    :_id
       :as   user} datetime]
  (let [date (keyword (date-format (time/local-date datetime)))
        time (time-format (time/local-time datetime))
        new  (if (contains? dates date)
               (assoc-in user [:dates date :punches] (conj (get-in user [:dates date :punches]) time))
               (assoc-in user [:dates date] {:punches [time]}))]
    (db/update-user db id new)))

(defn remove-punch
  "TODO"
  [db {dates :dates
       id    :_id
       :as   user} datetime]
  (let [date (keyword (date-format (time/local-date datetime)))
        time (time-format (time/local-time datetime))
        new  (if (contains? dates date)
               (assoc-in user [:dates date :punches] (remove (partial = time) (get-in user [:dates date :punches])))
               user)]
    (db/update-user db id new)))
