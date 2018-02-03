(ns hb.clara.schedule
  (:require [clara.rules.accumulators :as acc]
            [clara.rules :refer :all]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]))

(defn- datestamp
  [year month day]
  (-> (t/date-time year month day)
      (tc/to-long)))

;; Supplier submits a draft property, but never finishes it.
;; We need to poke him.

(defrecord Today [date])

(defrecord Notification [date])

(defrule each-day-notify-the-supplier
  "Each day send a notification to supplier he should finish updating his draft property"
  [Today (= ?date date)]
  =>
  (insert! (->Notification ?date)))

(defquery get-notifications
  []
  [?notification <- Notification])

(defn print-notifications
  [session]
  (println "Notifications are:")
  (let [notifications
        (->> (query session get-notifications) ; [{:?notification Notification}, {:?notification Notification},..]
             (map :?notification)
             (sort-by :date))]
    (doseq [notification notifications
            :let [{datestamp :date} notification
                  date (tc/from-long datestamp)
                  formatted-date (tf/unparse (tf/formatters :year-month-day) date)]]
      (println "Notification at" formatted-date))))

(defn run-examples
  []
  (-> (mk-session 'hb.clara.schedule)
      (insert (->Today (datestamp 2018 1 29)))
      (fire-rules)
      (print-notifications))
  nil)

;; Time for a real task.
;;
;; When a supplier does not log in after 3 days in a row,
;; start sending him notifications.
