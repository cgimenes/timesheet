(ns timesheet.services
  (:require [java-time :as time]
            [timesheet.db :as db]
            [timesheet.core :as core]))

(defn user-data
  "TODO"
  [{timesheet :timesheet}]
  (let [reduced-timesheet (core/timesheet-transducer timesheet)]
    {:timesheet reduced-timesheet
     :possible-departure (core/possible-departure reduced-timesheet core/today)}))

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
