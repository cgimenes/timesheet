(ns timesheet.services
  (:require [java-time :as time]
            [timesheet.db :as db]
            [timesheet.core :as core]))

(defn months-names
  "TODO"
  [months]
  (map (fn [x] (:month x)) months))

(defn user-data
  "TODO"
  [{timesheet :timesheet email :email}]
  (let [reduced-timesheet (core/timesheet-transducer timesheet)]
    {:timesheet reduced-timesheet
     :months (months-names (:months reduced-timesheet))
     :possible-departure (core/possible-departure reduced-timesheet core/today)
     :email email}))

(defn add-punch
  "TODO"
  [db {timesheet :timesheet id :_id :as user} datetime]
  (db/update-user db id (assoc user :timesheet (core/add-punch timesheet
                                                               (time/local-date datetime)
                                                               (time/local-time datetime)))))

(defn remove-punch
  "TODO"
  [db {timesheet :timesheet id :_id :as user} datetime]
  (db/update-user db id (assoc user :timesheet (core/remove-punch timesheet
                                                                  (time/local-date datetime)
                                                                  (time/local-time datetime)))))
