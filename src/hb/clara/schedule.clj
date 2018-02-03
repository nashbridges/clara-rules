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
;; Notifications should not be sent on Friday, Saturday and Sunday.
;; Notifcations should be sent as
;;   type A
;;   type B
;;   type A
;;   type B
;;   type A

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

(defrecord Weekday [date])

(defn- weekday?
  [datestamp]
  (let [date (tc/from-long datestamp)]
    (#{1 2 3 4} (t/day-of-week date))))

(defrule weekday
  "Weekday is any day except Friday, Saturday and Sunday"
  [Today (= ?date date)
         (weekday? ?date)]
  =>
  (insert! (->Weekday ?date)))

(defrule on-weekday-notify-after-3-days-after-last-login
  "When 3 days pass after last login, send a notification on a weekday"
  [LastLogin (= ?login-date date)]
  [Weekday (= ?today-date date)
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

(defrecord NotificationA [date])
(defrecord NotificationB [date])

(defrule notification-a-first
  "First notification should be of type A"
  [Notification (= ?date date)]
  [:not [NotificationA]]
  =>
  (insert-unconditional! (->NotificationA ?date)))

(defrule notification-b-second
  "Second notification should be of type B"
  [Notification (= ?date date)]
  [NotificationA (> ?date date)]
  [:not [NotificationB]]
  =>
  (insert-unconditional! (->NotificationB ?date)))

(defrecord LastNotificationA [date])
(defrecord LastNotificationB [date])

(defrule last-notification-a
  (?date <- (acc/max :date) :from [NotificationA])
  =>
  (insert-unconditional! (->LastNotificationA ?date)))

(defrule last-notification-b
  (?date <- (acc/max :date) :from [NotificationB])
  =>
  (insert-unconditional! (->LastNotificationB ?date)))

(defrule notification-a-after-b
  "Notification A should be after one of type B"
  [Notification (= ?date date)]
  [LastNotificationA (= ?a-date date)
                     (> ?date ?a-date)]
  [LastNotificationB (= ?b-date date)
                     (> ?date ?b-date)
                     (< ?a-date ?b-date)]
  =>
  (insert! (->NotificationA ?date)))

(defrule notification-b-after-a
  "Notification B should be after one of type A"
  [Notification (= ?date date)]
  [LastNotificationA (= ?a-date date)
                     (> ?date ?a-date)]
  [LastNotificationB (= ?b-date date)
                     (> ?date ?b-date)
                     (> ?a-date ?b-date)]
  =>
  (insert! (->NotificationB ?date)))

(defquery get-notifications-a
  []
  [?notification <- NotificationA])

(defquery get-notifications-b
  []
  [?notification <- NotificationB])

(defn print-notifications
  [session]
  (println "Notifications are:")
  (let [notifications
        (->> (concat (query session get-notifications-a) (query session get-notifications-b))
             (map :?notification)
             (sort-by :date))]
    (doseq [notification notifications
            :let [{datestamp :date} notification
                  date (tc/from-long datestamp)
                  formatted-date (tf/unparse (tf/formatters :year-month-day) date)
                  notification-type (if (= (type notification) NotificationA) "A" "B")]]
      (println "Notification" notification-type "at" formatted-date))))

(defn run-examples
  []

  ;; Next day after login

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 1 30)))
      (fire-rules)
      (print-notifications))

  ;; Three days after login (but it's Friday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 2 2)))
      (fire-rules)
      (print-notifications))

  ;; Four days after login (but it's Saturday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 2 3)))
      (fire-rules)
      (print-notifications))

  ;; Five days after login (but it's Sunday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 2 4)))
      (fire-rules)
      (print-notifications))

  ;; Six days after login (it's Monday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->Today (datestamp 2018 2 5)))
      (fire-rules)
      (print-notifications))

  ;; Seven days after login (it's Tuesday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->NotificationA (datestamp 2018 2 5))
              (->Today (datestamp 2018 2 6)))
      (fire-rules)
      (print-notifications))

  ;; Eight days after login (it's Wednesday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->NotificationA (datestamp 2018 2 5))
              (->NotificationB (datestamp 2018 2 6))
              (->Today (datestamp 2018 2 7)))
      (fire-rules)
      (print-notifications))

  ;; Nine days after login (it's Thursday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 29))
              (->NotificationA (datestamp 2018 2 5))
              (->NotificationB (datestamp 2018 2 6))
              (->NotificationA (datestamp 2018 2 7))
              (->Today (datestamp 2018 2 8)))
      (fire-rules)
      (print-notifications))

  ;; Fifth notification (but it's on Friday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 25))
              (->NotificationA (datestamp 2018 1 29))
              (->NotificationB (datestamp 2018 1 30))
              (->NotificationA (datestamp 2018 1 31))
              (->NotificationB (datestamp 2018 2 1))
              (->Today (datestamp 2018 2 2)))
      (fire-rules)
      (print-notifications))

  ;; Fifth notification (it's on Monday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 25))
              (->NotificationA (datestamp 2018 1 29))
              (->NotificationB (datestamp 2018 1 30))
              (->NotificationA (datestamp 2018 1 31))
              (->NotificationB (datestamp 2018 2 1))
              (->Today (datestamp 2018 2 5)))
      (fire-rules)
      (print-notifications))

  ;; Sixth notification (on Tuesday)

  (-> (mk-session 'hb.clara.schedule)
      (insert (->Login (datestamp 2018 1 25))
              (->NotificationA (datestamp 2018 1 29))
              (->NotificationB (datestamp 2018 1 30))
              (->NotificationA (datestamp 2018 1 31))
              (->NotificationB (datestamp 2018 2 1))
              (->NotificationA (datestamp 2018 2 5))
              (->Today (datestamp 2018 2 6)))
      (fire-rules)
      (print-notifications))
  nil)
