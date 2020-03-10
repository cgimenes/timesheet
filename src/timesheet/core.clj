(ns timesheet.core
  (:gen-class)
  (import java.time.YearMonth)
  (:require [timesheet.filters :refer [month-format day-format dur-format time-format]]
            [timesheet.middlewares :refer [wrap-db wrap-session]]
            [java-time :as time]
            [clojure.pprint :refer [pprint]]
            [ring.adapter.jetty :as jetty]
            [compojure.core :refer [defroutes GET POST DELETE]]
            [compojure.route :as route]
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
      {:punches punches
       :worked zero
       :left zero
       :balance zero
       :review true
       :not-calculated not-calculated}
      (reduce (fn [{worked :worked left :left balance :balance review :review} [a b]]
                (let [diff (time/duration (truncate a) (truncate b))]
                  {:punches punches
                   :worked (time/plus worked diff)
                   :left (positive-minus left diff)
                   :balance (time/plus balance diff)
                   :review false
                   :not-calculated not-calculated}))
              {:punches punches
               :worked zero
               :left (if not-calculated zero workday)
               :balance (if not-calculated zero (time/negate workday))
               :review false
               :not-calculated not-calculated}
              (->> punches
                   (map time/local-time)
                   (partition 2 2 [now]))))
    ))

(defn assoc-merge
  "TODO"
  [m f k v]
  (if (contains? m k)
    (assoc m k (f (get m k) v))
    (assoc m k v)))

(defn month-merger
  "TODO"
  [a b]
  {:days (merge (:days a) (:days b))
   :balance (time/plus (:balance a) (:balance b))})
  
(defn complete-month-days
  "TODO"
  [month]
  (into {} (let [date (apply time/local-date (map read-string (str/split month #"-")))
                 month-days (take
                             (.lengthOfMonth (YearMonth/of (time/as date :year) (time/as date :month-of-year)))
                             (time/iterate time/plus date (time/days 1)))]
             (map (fn [x] {(keyword (str x)) {:punches []}}) month-days))))

(defn timesheet-reducer
  "TODO"
  [{total-balance :balance months :months} [k {punches :punches}]]
  (let [date (time/local-date (name k))
        {day-balance :balance :as day} (reduce-day date punches)
        {month-balance :balance :as month} {:days (sorted-map (time/as date :day-of-month) day)
                                            :balance day-balance}]
    {:balance (time/plus total-balance month-balance)
     :months (assoc-merge months
                          month-merger
                          (month-format date)
                          month)}))

(defn reduce-timesheet
  "TODO"
  [dates]
  (as-> dates $
    (reduce timesheet-reducer {:balance zero :months (sorted-map)} $)
    (reduce timesheet-reducer $ (into (sorted-map) (map complete-month-days (keys (:months $)))))))

(defn user-data
  "TODO"
  [{dates :dates email :email}]
  (let [timesheet (reduce-timesheet dates)] {:timesheet timesheet
                                             :months (keys (:months timesheet))
                                             :possible-departure (time/local-time)
                                             :email email}))

; TODO
; * Fix month balance
; * Possible departure
; * Allowances
; * Hide month

(defn get-user
  "TODO"
  [db email]
  (mc/find-one-as-map db "users" {:email email}))

(add-filter! :month-str month-format)

(add-filter! :day-str day-format)

(add-filter! :dur-str dur-format)

(add-filter! :time-str time-format)

(defroutes app
  (POST "/logout" [] (tmpl/render-file "templates/logout.html"))
  (wrap-db
   (wrap-session
    (POST "/timesheet" {:keys [db email]}
      ())))
  (wrap-db
   (wrap-session
      (GET "/timesheet" {:keys [db email]}
        (tmpl/render-file "templates/timesheet.html" (user-data (get-user db email))))))
  (route/not-found "<h1>Page not found</h1>"))

(:body (app {:uri "/timesheet" :request-method :get}))
;; (:body (app {:uri "/timesheet" :query-string "year=2020&month=2" :request-method :post}))

(defn -main
  "TODO"
  []
  (jetty/run-jetty app {:port 8080}))

;; (let [review (should-review-day date)])
;; (-> day
;;     (update :date time/local-date)
;;     (assoc :worked (calc-if-not-review calc-worked punches review))
;;     (assoc :left (calc-if-not-review calc-left punches review))
;;     (assoc :balance (calc-if-not-review calc-balance punches review))
;;     (assoc :review review))

;; (defn month-data
;;   "TODO"
;;   [month days]
;;   (as-> (vai month days) $
;;     {:month month :days (map day-data $)}
;;     (assoc $ :balance (calc-month-balance (:days $)))))


;; (defn calc-worked
;;   "TODO"
;;   [punches]
;;   (->> punches
;;        (map time/local-time)
;;        (partition 2 2 [(time/local-time)])
;;        (map (fn [[a b]] (time/duration (truncate a) (truncate b))))
;;        (reduce time/plus)))

;; (defn calc-balance
;;   "TODO"
;;   [punches]
;;   (let [left (calc-left punches)]
;;     (if (> (time/as left :minutes) 0)
;;       (time/negate left)
;;       (time/minus (calc-worked punches) workday))))

;; (def calc-month-balance
;;   "TODO"
;;   #(reduce time/plus (map :balance %)))

;; (defn calc-if-not-review
;;   "TODO"
;;   [calc punches review]
;;   (if (not review)
;;     (calc punches)
;;     zero))