(ns shared-buffer-server.history
  (:require [clojure.set :refer [intersection]]
            [shared-buffer-server.operations :refer :all]))

(defn concurrent?
  "Returns non-nil iff the events are concurrent."
  [[_ t1 m1 u1] [_ t2 m2 u2]]
  (and (not= u1 u2)
       (or (and (<= t1 t2) (<= t2 m1))
           (and (<= t2 t1) (<= t1 m2)))))

(defn precedes?
  "Returns non-nil iff the e1 precedes e2."
  [[[op1] _ m1 _ :as e1] [[op2] _ m2 _ :as e2]]
  (if (concurrent? e1 e2)
    (or (> (:pos op1) (:pos op2))
        (and (= (:pos op1) (:pos op2)) (:del op1))
        (and (= (:pos op1) (:pos op2))
             (= (keys op1) (keys op2))
             (< m1 m2)))
    (< m1 m2)))

(defn trim-history [history min-token]
  (take-while (fn [[_ t _ _]] (> t min-token)) history))

(defn add-event
  "Adds an event to the history."
  [history [op1 _ _ u1 :as e1]]
  (let [u?      (comp (partial = u1) last)
        [xs ys] (split-with u? history)
        [x y]   (split-with (partial precedes? e1) xs)]
    (concat x [e1] y ys)))

(defn until
  "Get the history until state t."
  [history t]
  (->> (reverse history)
       (drop-while (fn [[_ _ m _]] (< m t)))
       (mapcat first) reverse))

(defn rejected [r t]
  (mapcat first (take-while (fn [[_ m]] (< t m)) r)))

(defn make-op [h1 h2 t]
  (simplify (compose (until h2 t) (inv (until h1 t)))))

(defn make-response [op1 op2 sent-ops t]
  (simplify (compose op2 (compose (rejected sent-ops t) (inv op1)))))

(defn make-initial-op [op1 sent-ops t]
  (simplify (compose (rejected sent-ops t) op1)))
