(ns timesheet.main
  (:gen-class)
  (:require [timesheet.middlewares :refer [wrap-db wrap-session wrap-db-user]]
            [timesheet.services :refer [remove-punch add-punch user-data]]
            [java-time :as time]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-response]]
            [compojure.core :refer [defroutes context GET POST DELETE]]
            [compojure.route :as route]
            [ring.middleware.cors :refer [wrap-cors]]
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

(defroutes app
  (wrap-cors
   (wrap-db
    (wrap-session
     (wrap-params
      (wrap-db-user
       (wrap-json-response
        (context "/api" []
          (context "/timesheet" []
            (DELETE "/" {:keys [db user form-params]}
              (remove-punch db user (time/local-date-time (get form-params "datetime"))))
            (POST "/" {:keys [db user form-params]}
              (add-punch db user (time/local-date-time (get form-params "datetime"))))
            (GET "/" {:keys [user]}
              (response (user-data user))))))))))
   :access-control-allow-methods [:get :post :delete]
   :access-control-allow-origin #".*")
  (route/not-found nil))

(defn -main
  "TODO"
  []
  (jetty/run-jetty app {:port 8082}))
