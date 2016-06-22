(ns shared-buffer-server.operations
  (:require [clojure.set :refer [rename-keys]]))

(def nop? :nop)

(defn inv
  "Returns the inverse of a given operation."
  [x]
  (if (seq? x)
    (map inv (reverse x))
    (rename-keys x {:ins :del, :del :ins})))

(defn inverses? [x y]
  (= (inv x) y))

(defn compose
  "Takes two operations, and returns their composition."
  [x y]
  (cond (nop? x) y
        (nop? y) x
        (inverses? x y) (list {:nop true})
        :else (concat x y)))

(defn simplify [op]
  (-> (fn [stack o]
        (cond (nop? o) stack
              (empty? stack) (conj stack o)
              (inverses? (peek stack) o) (pop stack)
              :else (conj stack o)))
      (reduce [] op) seq))
