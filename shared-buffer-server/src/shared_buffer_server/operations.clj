(ns shared-buffer-server.operations
  (:require [clojure.set :refer [rename-keys]]))

(def nop? :nop)

(defn inv
  "Returns the inverse of x."
  [x]
  (if (seq? x)
    (map inv (reverse x))
    (rename-keys x {:ins :del, :del :ins})))

(defn inverses?
  "Returns true if x is the inverse of y"
  [x y]
  (= x (inv y)))

(defn compose
  "Takes two operations, and returns their composition."
  [x y]
  (concat x y))

(defn simplify [op]
  "Reduces op it to a minimal form. It removes nop elements and adjacent
  operations that are inverses of each other."
  (-> (fn [stack o]
        (cond (nop? o) stack
              (empty? stack) (conj stack o)
              (inverses? (peek stack) o) (pop stack)
              :else (conj stack o)))
      (reduce [] op) seq))
