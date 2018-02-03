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

;; When a supplier does not log in after 3 days in a row,
;; start sending him notifications, but send no more than 5.

(defrecord Today [date])

(defrecord Notification [date])

(defrecord Login [date])

(defn- days-between
  [start-datestamp end-datestamp]
  (let [start (tc/from-long start-datestamp)
        end (tc/from-long end-datestamp)]
    (if (t/before? start end)
      (t/in-days (t/interval start end))
      -1)))

(defrecord LastLogin [date])

(defrule notify-after-3-days-after-last-login
  "When 3 days pass after last login, send a notification"
  [LastLogin (= ?login-date date)]
  [Today (= ?today-date date)
         (> (days-between ?login-date ?today-date) 3)]
  =>
  (insert! (->Notification ?today-date)))

(defrule last-login
  (?login-date <- (acc/max :date) :from [Login])
  =>
  (insert! (->LastLogin ?login-date)))

(defrecord NotificationTotal [total])

(defrule notification-total
  (?total <- (acc/count) :from [Notification])
  =>
  (insert! (->NotificationTotal ?total)))

(defrecord LastNotification [date])

(defrule last-notification
  (?date <- (acc/max :date) :from [Notification])
  =>
  (insert! (->LastNotification ?date)))

(defrule no-more-than-5-notifications
  "Do not send more than 5 notifications in order to not overspam suppliers"
  [NotificationTotal (> total 5)]
  [LastNotification (= ?date date)]
  =>
  (retract! (->Notification ?date)))

(defquery get-notifications
  []
  [?notification <- Notification])

(defn print-notifications
  [session]
  (println "Notifications are:")
  (let [notifications
        (->> (query session get-notifications)
             (map :?notification)
             (sort-by :date))]
    (doseq [notification notifications
            :let [{datestamp :date} notification
                  date (tc/from-long datestamp)
                  formatted-date (tf/unparse (tf/formatters :year-month-day) date)]]
      (println "Notification at" formatted-date))))

(defn run-examples
  []

  ;; Next day after login

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 1 30)))
      (fire-rules)
      (print-notifications))

  ;; Three days after login

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 2 2)))
      (fire-rules)
      (print-notifications))

  ;; Four days after login

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 2 3)))
      (fire-rules)
      (print-notifications))

  ;; But what if there were several logins?

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Login (datestamp 2018 2 1))
              (->Today (datestamp 2018 2 3)))
      (fire-rules)
      (print-notifications))

  ;; Fifth notification

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Notification (datestamp 2018 2 2))
              (->Notification (datestamp 2018 2 3))
              (->Notification (datestamp 2018 2 4))
              (->Notification (datestamp 2018 2 5))
              (->Today (datestamp 2018 2 6)))
      (fire-rules)
      (print-notifications))

  ;; Sixth notification

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Notification (datestamp 2018 2 2))
              (->Notification (datestamp 2018 2 3))
              (->Notification (datestamp 2018 2 4))
              (->Notification (datestamp 2018 2 5))
              (->Notification (datestamp 2018 2 6))
              (->Today (datestamp 2018 2 7)))
      (fire-rules)
      (print-notifications))

  ;; Should send notification only on weekdays

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 2 3))) ; Saturday
      (fire-rules)
      (print-notifications))
  nil)

;; When a supplier does not log in after 3 days in a row,
;; start sending him notifications, but send no more than 5.
;; Notifications should not be sent on Friday, Saturday and Sunday.
