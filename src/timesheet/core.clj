(ns timesheet.core
  (:gen-class)
  (import java.time.YearMonth)
  (:require [timesheet.filters :refer [month-format day-format dur-format time-format date-format]]
            [timesheet.middlewares :refer [wrap-db wrap-session]]
            [java-time :as time]
            [clojure.data.json :as json]
            [clojure.pprint :refer [pprint]]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :as response]
            [compojure.core :refer [defroutes context GET POST DELETE]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [monger.collection :as mc]
            [clojure.string :as str]
            [selmer.parser :as tmpl]
            [selmer.filters :refer [add-filter!]]))

(def workday
  "normal workday duration"
  (time/duration 8 :hours))

(def zero 
  "a zeroed duration"
  (time/duration))

(def truncate
  "truncate duration to minutes"
  #(time/truncate-to % :minutes))

(def today
  "today's date"
  (time/local-date))

(def now
  "now's time"
  (time/local-time))

(defn should-review
  "returns true if it's an incomplete day (with odd) and date is before today"
  [date punches]
  (and ((comp not even? count) punches)
       (time/before? date today)))

(defn positive-minus
  "returns the difference between two durations only if the difference is greater than zero, 
   otherwise returns a zeroed duration"
  [a b]
  (let [diff (time/minus a b)] (if (time/negative? diff)
                                 zero
                                 diff)))

(defn reduce-day
  "TODO"
  [date punches]
  (let [not-calculated (or (time/weekend? date) (time/after? date today))]
    (if (should-review date punches)
      {:punches        punches
       :worked         zero
       :left           zero
       :balance        zero
       :review         true
       :not-calculated not-calculated}
      (reduce (fn [{worked  :worked
                    left    :left
                    balance :balance
                    review  :review} [a b]]
                (let [diff (time/duration (truncate a) (truncate b))]
                  {:punches        punches
                   :worked         (time/plus worked diff)
                   :left           (positive-minus left diff)
                   :balance        (time/plus balance diff)
                   :review         false
                   :not-calculated not-calculated}))
              {:punches        punches
               :worked         zero
               :left           (if not-calculated zero workday)
               :balance        (if not-calculated zero (time/negate workday))
               :review         false
               :not-calculated not-calculated}
              (->> punches
                   (map time/local-time)
                   (partition 2 2 [now]))))))

(defn assoc-merge
  "TODO"
  [m f k v]
  (if (contains? m k)
    (assoc m k (f (get m k) v))
    (assoc m k v)))

(defn month-merger
  "TODO"
  [& months]
  {:days    (reduce merge (reverse (map :days months)))
   :balance (reduce time/plus (map :balance months))})
  
(defn complete-month-days
  "TODO"
  [month]
  (into {} (let [date       (apply time/local-date (map read-string (str/split month #"-")))
                 month-days (take
                             (.lengthOfMonth (YearMonth/of (time/as date :year) (time/as date :month-of-year)))
                             (time/iterate time/plus date (time/days 1)))]
             (map (fn [x] {(keyword (str x)) {:punches []}}) month-days))))

(defn months-contains-date
  [months date]
  (let [month-number (month-format date)
        day-number   (time/as date :day-of-month)]
    (and
     (contains? months month-number)
     (contains? (:days (get months month-number)) day-number))))

(defn timesheet-reducer
  "TODO"
  [{total-balance :balance
    months        :months
    :as           timesheet} [k {punches :punches}]]
  (let [date (time/local-date (name k))]
    (if (months-contains-date months date)
      timesheet
      (let [{day-balance :balance
             :as         day}     (reduce-day date punches)
            month-number                        (month-format date)
            day-number                          (time/as date :day-of-month)
            {month-balance :balance
             :as           month} {:days    (sorted-map day-number day)
                                   :balance day-balance}]
        {:balance (time/plus total-balance month-balance)
         :months  (assoc-merge months
                               month-merger
                               month-number
                               month)}))))

(defn reduce-timesheet
  "TODO"
  [dates]
  (as-> dates $
    (reduce timesheet-reducer {:balance zero
                               :months  (sorted-map)} $)
    (reduce timesheet-reducer $ (into (sorted-map) (map complete-month-days (keys (:months $)))))))

(defn possible-departure
  "TODO"
  [left]
  (time/plus now left))

(defn user-data
  "TODO"
  [{dates :dates
    email :email}]
  (let [timesheet (reduce-timesheet dates)] {:timesheet          timesheet
                                             :months             (keys (:months timesheet))
                                             :possible-departure (-> timesheet
                                                                     :months
                                                                     (get (month-format today))
                                                                     :days
                                                                     (get (time/as today :day-of-month))
                                                                     :left
                                                                     possible-departure)
                                             :email              email}))

; TODO
; * Allowances
; * * Show
; * * Add
; * Hide month
; * Auth
; * Export old timesheet
; * That refactor

(defn get-user
  "TODO"
  [db email]
  (mc/find-one-as-map db "users" {:email email}))

(add-filter! :month-str month-format)

(add-filter! :day-str day-format)

(add-filter! :dur-str dur-format)

(add-filter! :time-str time-format)

(defn add-punch
  "TODO"
  [db {dates :dates
       id    :_id
       :as   user} datetime]
  (let [date (keyword (date-format (time/local-date datetime)))
        time (time-format (time/local-time datetime))
        new  (if (contains? dates date)
               (assoc-in user [:dates date :punches] (conj (get-in user [:dates date :punches]) time))
               (assoc-in user [:dates date] {:punches [time]}))]
    (mc/update-by-id db "users" id new)))

(defn remove-punch
  "TODO"
  [db {dates :dates
       id    :_id
       :as   user} datetime]
  (let [date (keyword (date-format (time/local-date datetime)))
        time (time-format (time/local-time datetime))
        new  (if (contains? dates date)
               (assoc-in user [:dates date :punches] (remove (partial = time) (get-in user [:dates date :punches])))
               user)]
    (mc/update-by-id db "users" id new)))

(defroutes app
  (POST "/logout" [] (tmpl/render-file "templates/logout.html"))
  (wrap-db
   (wrap-session
    (wrap-params
     (context "/timesheet" []
       (GET "/remove" {:keys [db email query-params]} (do (remove-punch db (get-user db email) (time/local-date-time (get query-params "datetime")))
                                                                    (response/redirect "/timesheet" :see-other)))
       (POST "/" {:keys [db email form-params]} (do (add-punch db (get-user db email) (time/local-date-time (get form-params "datetime")))
                                                             (response/redirect "/timesheet" :see-other)))
       (GET "/" {:keys [db email]}
         (tmpl/render-file "templates/timesheet.html" (user-data (get-user db email))))))))
  (route/not-found "<h1>Page not found</h1>"))

;; (:body (app {:uri "/timesheet" :request-method :get})))
;; (:body (app {:uri "/timesheet/remove" :query-params {"datetime" "2020-03-12T10:45:00"} :request-method :get}))
;; (:body (app {:uri "/timesheet" :form-params {"datetime" "2020-03-12T13:06:00"} :request-method :post}))

(defn -main
  "TODO"
  []
  (jetty/run-jetty app {:port 8080}))
