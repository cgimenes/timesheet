(ns timesheet.middlewares
  (:gen-class)
  (:require [java-time :as time]
            [monger.core :as mg]
            [monger.credentials :as mcr]))

(defn wrap-session
  "TODO"
  [handler]
  (fn [request]
    (handler (-> request
                 (assoc :email "marcelo.gimenes.oliveira@gmail.com")))))

(defn wrap-db
  "TODO 'localhost, default port'"
  [handler]
  (let [admin-db   "admin"
        user    "root"
        password    (.toCharArray "example")
        cred (mcr/create user admin-db password)
        host "127.0.0.1"
        conn (mg/connect-with-credentials host cred)
        db   (mg/get-db conn "timesheet")]
    (fn [request]
      (handler (-> request
                   (assoc :db db))))))

; (defn wrap-date 
;   "TODO"
;   [handler]
;   (fn [request]
;     (let [year (-> request :query-params (get "year") read-string)
;           month (-> request :query-params (get "month") read-string)]
;       (handler (-> request
;                    (assoc :month (time/local-date year month)))))))