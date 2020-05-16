(ns timesheet.main
  (:gen-class)
  (:require [timesheet.middlewares :refer [wrap-db wrap-session wrap-db-user]]
            [timesheet.filters :refer [day-format dur-format month-format time-format]]
            [timesheet.services :refer [remove-punch add-punch user-data]]
            [java-time :as time]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :as response]
            [compojure.core :refer [defroutes context GET POST]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [selmer.parser :as tmpl]
            [selmer.filters :refer [add-filter!]]))

(add-filter! :month-str month-format)
(add-filter! :day-str day-format)
(add-filter! :dur-str dur-format)
(add-filter! :time-str time-format)

(defroutes app
  (POST "/logout" [] (tmpl/render-file "templates/logout.html"))
  (wrap-db
   (wrap-session
    (wrap-params
     (wrap-db-user
      (context "/timesheet" []
        (GET "/remove" {:keys [db user query-params]} (do (remove-punch db user (time/local-date-time (get query-params "datetime")))
                                                          (response/redirect "/timesheet" :see-other)))
        (POST "/" {:keys [db user form-params]} (do (add-punch db user (time/local-date-time (get form-params "datetime")))
                                                    (response/redirect "/timesheet" :see-other)))
        (GET "/" {:keys [user]}
          (tmpl/render-file "templates/timesheet.html" (user-data user))))))))
  (route/not-found "<h1>Page not found</h1>"))

(defn -main
  "TODO"
  []
  (jetty/run-jetty app {:port 8080}))
