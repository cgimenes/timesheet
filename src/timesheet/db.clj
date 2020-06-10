(ns timesheet.db
  (:require [monger.collection :as mc]
            [monger.conversion :refer [ConvertToDBObject ConvertFromDBObject]]
            [java-time :as time]))

(extend-protocol ConvertToDBObject
  java.time.Duration
  (monger.conversion/to-db-object [^Duration input]
    (monger.conversion/to-db-object (.toString input))))

(extend-protocol ConvertFromDBObject
  java.util.Date
  (from-db-object [input keywordize] (let [time (.getTime input)]
                                       (if (> time 86340000)
                                         (time/local-date time (time/zone-id "UTC"))
                                         (time/local-time time (time/zone-id "UTC")))))
  
  String
  (from-db-object [input keywordize] (try 
                                       (time/duration input) 
                                       (catch clojure.lang.ExceptionInfo _
                                         input))))

(defn get-user
  "TODO"
  [db email]
  (mc/find-one-as-map db "users" {:email email}))

(defn update-user
  "TODO"
  [db id new]
  (mc/update-by-id db "users" id new))