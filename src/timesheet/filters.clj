(ns timesheet.filters
  (:require [java-time :as time]))

(defn dur-format
  "TODO"
  [duration]
  (let [template (if (time/negative? duration) "-%02d:%02d" "%02d:%02d")
        duration (if (time/negative? duration) (time/negate duration) duration)]
    (format template
            (quot (time/as duration :minutes) 60)
            (mod (time/as duration :minutes) 60))))

(def month-format
  "TODO"
  (partial time/format "yyyy-MM"))

(def day-format
  "TODO"
  (partial time/format "dd"))

(def time-format
  "TODO"
  (partial time/format "HH:mm"))

(def date-format
  "TODO"
  (partial time/format "yyyy-MM-dd"))
