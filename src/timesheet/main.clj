(ns timesheet.main
  (:gen-class)
  (:require [timesheet.middlewares :refer [wrap-db wrap-session wrap-db-user]]
            [timesheet.services :refer [remove-punch add-punch user-data]]
            [java-time :as time]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-response]]
            [compojure.core :refer [defroutes context GET POST OPTIONS]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [cheshire.generate]))

(extend-protocol cheshire.generate/JSONable
  java.time.LocalDate
  (to-json [dt gen]
    (cheshire.generate/write-string gen (str dt)))
  java.time.Duration
  (to-json [dt gen]
    (cheshire.generate/write-string gen (str dt)))
  java.time.LocalTime
  (to-json [dt gen]
    (cheshire.generate/write-string gen (str dt))))

(use 'debux.core)

(defroutes app
  (context "/api" []
    (context "/timesheet" []
      (OPTIONS "/" {:status 200
                    :headers {"Access-Control-Allow-Methods" "POST, GET"
                              "Access-Control-Allow-Headers" "Authorization"}
                    :body ""})
      (wrap-db
       (wrap-session
        (wrap-params
         (wrap-db-user
          (wrap-json-response
           (GET "/remove" {:keys [db user query-params]} (remove-punch db user (time/local-date-time (get query-params "datetime")))))))))
      (wrap-db
       (wrap-session
        (wrap-params
         (wrap-db-user
          (wrap-json-response
           (POST "/" {:keys [db user form-params]} (add-punch db user (time/local-date-time (get form-params "datetime")))))))))
      (wrap-db
       (wrap-session
        (wrap-params
         (wrap-db-user
          (wrap-json-response
           (GET "/" {:keys [user]} (response (user-data user))))))))))
  (route/not-found ""))

(defn -main
  "TODO"
  []
  (jetty/run-jetty app {:port 8082}))
