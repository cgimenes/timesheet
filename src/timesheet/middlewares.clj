(ns timesheet.middlewares
  (:gen-class)
  (:require [monger.core :as mg]
            [monger.credentials :as mcr]
            [timesheet.db :as db]
            [buddy.auth.middleware :refer [wrap-authentication]]
            [buddy.auth.backends :as backends]
            [clj-http.client :as client]))

(use 'debux.core)

(defn get-user-email
  [token]
  (dbg (let [response (client/get "https://dev-03ovpnrf.us.auth0.com/userinfo" {:headers {"Authorization" (str "Bearer " token)}
                                                                                :as :json
                                                                                :throw-exceptions false})]
         (when (= 200 (:status response))
           (let [{email :email} (:body response)]
             email)))))

(defn authentication
  [_ token]
  (dbg token)
  (let [token (keyword token)]
    (get-user-email token)))

(defn wrap-session
  "TODO"
  [handler]
  (fn [request]
    (wrap-authentication (handler (-> request
                                      (assoc :email (:identity request))))
                         (backends/token {:authfn authentication}))))

(defn wrap-db-user
  "TODO"
  [handler]
  (fn [request]
    (handler (-> request
                 (assoc :user (db/get-user (:db request) (:email request)))))))

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