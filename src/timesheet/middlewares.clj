(ns timesheet.middlewares
  (:gen-class)
  (:require [monger.core :as mg]
            [monger.credentials :as mcr]
            [timesheet.db :as db]
            [buddy.auth.middleware :refer [wrap-authentication]]
            [buddy.auth.backends :as backends]
            [clj-http.client :as client]))

(defn get-user-email
  [token]
  (let [response (client/get "https://dev-03ovpnrf.us.auth0.com/userinfo" {:headers {"Authorization" (str "Bearer " token)}
                                                                           :as :json
                                                                           :throw-exceptions false})]
    (when (= 200 (:status response))
      (let [{email :email} (:body response)]
        email))))

(defn wrap-session
  "TODO"
  [handler] 
  (wrap-authentication (fn [request] (handler (-> request
                                                  (assoc :email (:identity request)))))
                       (backends/token {:authfn #(get-user-email %2)})))

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
