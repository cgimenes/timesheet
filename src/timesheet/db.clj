(ns timesheet.db
  (:require [monger.collection :as mc]))

(defn get-user
  "TODO"
  [db email]
  (mc/find-one-as-map db "users" {:email email}))

(defn update-user
  "TODO"
  [db id new]
  (mc/update-by-id db "users" id new))