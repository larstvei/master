(ns shared-buffer-server.history
  (:require [clojure.set :refer [intersection]]))

(defn concurrent?
  "Returns non-nil iff the events are concurrent."
  [[o1 i1 m1 u1] [o2 i2 m2 u2]]
  (and (empty? (intersection u1 u2))
       (or (and (<= i1 i2) (<= i2 m1))
           (and (<= i2 i1) (<= i1 m2)))))

(defn precedes?
  "Returns non-nil iff the e1 precedes e2."
  [[o1 i1 m1 u1 :as e1] [o2 i2 m2 u2 :as e2]]
  (if (concurrent? e1 e2)
    (or (> (:pos o1) (:pos o2))
        (and (= (:pos o1) (:pos o2)) (:del o1))
        (and (= (:pos o1) (:pos o2))
             (= (keys o1) (keys o2))
             (< m1 m2)))
    (< m1 m2)))

(defn add-event
  "Adds an event to the history."
  [history [o i m u :as e1]]
  (let [not-u? (comp not-empty (partial intersection u) last)
        xs     (take-while not-u? history)
        ys     (drop-while not-u? history)
        [x y]  (split-with (partial precedes? e1) xs)]
    (concat x [e1] y ys)))
