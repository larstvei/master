(ns shared-buffer-server.history
  (:require [clojure.set :refer [intersection]])
  (:require [shared-buffer-server.operations :refer :all]))

(defn concurrent?
  "Returns non-nil iff the events are concurrent."
  [[_ i1 m1 u1] [_ i2 m2 u2]]
  (and (empty? (intersection u1 u2))
       (or (and (<= i1 i2) (<= i2 m1))
           (and (<= i2 i1) (<= i1 m2)))))

(defn precedes?
  "Returns non-nil iff the e1 precedes e2."
  [[op1 _ m1 _ :as e1] [op2 _ m2 _ :as e2]]
  (if (concurrent? e1 e2)
    (or (> (:pos op1) (:pos op2))
        (and (= (:pos op1) (:pos op2)) (:del op1))
        (and (= (:pos op1) (:pos op2))
             (= (keys op1) (keys op2))
             (< m1 m2)))
    (< m1 m2)))

(defn add-event
  "Adds an event to the history."
  [history [_ _ _ u :as e1]]
  (let [not-u? (comp not-empty (partial intersection u) last)
        xs     (take-while not-u? history)
        ys     (drop-while not-u? history)
        [x y]  (split-with (partial precedes? e1) xs)]
    (concat x [e1] y ys)))